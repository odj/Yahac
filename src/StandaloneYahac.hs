{-# LANGUAGE ScopedTypeVariables #-}
import Yahac.Asteroids.Object.Render.Cairo
import Yahac.Asteroids.Game
import Yahac.Asteroids.Object.Basic
import Yahac.Asteroids.Object.Player
import Yahac.Asteroids.Object.Path
import Yahac.Asteroids.Game.Control
import Yahac.Asteroids.Internal.Engine
import Yahac.Asteroids.Internal.Color

import Data.Time
import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Graphics.UI.Gtk hiding (Color)
import Graphics.Rendering.Cairo
import qualified Data.Map as M
import Control.Monad.Writer
import Unsafe.Coerce
import Text.Printf


main = do
    _ <- initGUI

    (getTimeoutId, setTimeoutId) <- getAndSet Nothing
    mainWindow <- windowNew
    drawingArea <- drawingAreaNew

    renderWindow <- do
        let dr = drawingArea
        widgetAddEvents dr [PointerMotionMask]
        af <- aspectFrameNew 0.5 0.5 (Just 1)
        containerAdd af dr
        return af

    -- Create a basic game and single control
    -- Also create FPS and physics burn-ins
    game <- newGame
    mvGame <- newMVar game
    mvState <- newMVar defaultControlState
    mvFrameRate <- newEmptyMVar
    mvFps <- newMVar $ Fps [] 10
    mvRenderAve <- newMVar $ BoxAve [] 10

    let putGameState (g, s) = do
        putMVar mvGame g
        putMVar mvState s

    let getGameState = do
        g <- takeMVar mvGame
        s <- takeMVar mvState
        return (g, s)


    let pause = do 
        maybeId <- getTimeoutId
        case maybeId of 
            Just id -> do
                timeoutRemove id
                setTimeoutId Nothing
                return ()
            Nothing -> return ()


    let resume = do
        newId <- flip timeoutAdd 33  $ do
            widgetQueueDraw drawingArea
            return True
        _ <- setTimeoutId $ Just newId
        return ()


    let runUpdate u g = M.fold (\ c g -> u c =<< g) (pure g) (children g)


    let setNewPlayer = do
        player <- newPlayerWithLabel "player"
        path <- (newPathForUuid $ uuid player) 
        let green = Color 0.0 1.0 0.0 Nothing
        (game, state) <- getGameState
        let state' = defaultControlState { controlUuid = uuid player }
        let game' = addChild player $ addChild (path {lineColorFun = (\_ -> green) }) game
        putGameState (game', state')


    let updateDelta = 15 :: Int
    mvStop <- flip setInterval (fromIntegral updateDelta) $ do
            let update = do 
                    time0 <- getCurrentTime
                    (game, state) <- getGameState >>= flip ((onUpdate defaultControl) updateDelta) True

                    game' <- pure game >>= (runUpdate $ updateState updateDelta) >>= updateGame updateDelta
                                       >>= (runUpdate $ updatePosition updateDelta)
                    putGameState (game', state)

                    time1 <- getCurrentTime
                    let diff = fromIntegral $ unsafeCoerce (diffUTCTime time1 time0)
                        diffMs = diff / 1e9

                    pure mvRenderAve >>= takeMVar >>= (pure . addAvg diffMs)
                        >>= putMVar mvRenderAve

                    return ()


            -- This looks a little stupid.  It is a placeholder where we can
            -- conditionally throttle the game logic.  Currently does nothing.
            case True of
                True -> update
                False -> return ()

    buttons <- do
        qPause <- toggleButtonNewWithLabel stockMediaPause
        buttonSetUseStock qPause True
        onToggled qPause $ do
            p <- toggleButtonGetActive qPause
            case p of
                True -> pause
                False -> resume

        qNew <- buttonNewWithLabel "New Player"
        qQuit <- buttonNewWithLabel "Quit"
        qAbout <- buttonNewWithLabel "About"
        bb <- hButtonBoxNew
        containerAdd bb qNew
        containerAdd bb qPause
        containerAdd bb qAbout
        containerAdd bb qQuit

        _ <- onClicked qAbout about
        _ <- onClicked qQuit $ do
                putMVar mvStop True
                mainQuit

        _ <- onClicked qNew setNewPlayer
            
        return bb

    layout <- do
        vb <- vBoxNew False 0
        boxPackStart vb renderWindow PackGrow 0
        boxPackStart vb buttons PackNatural 0
        return vb


    _ <- drawingArea`on` exposeEvent $ tryEvent $ do
        (w, h) <- eventWindowSize
        liftIO $ do
            time0 <- getCurrentTime
            (game, state) <- getGameState
            putGameState (game, state)


            dw <- widgetGetDrawWindow drawingArea
            let rec = Rectangle 0 0 (floor w) (floor h)
            
            fps <- readMVar mvFps
            renderAvg <- readMVar mvRenderAve
            renderWithDrawable dw $ do
                scale w h
                background
                drawTextUL 0 $ show fps 
                drawTextUL 1 ("Total Objects: " ++ (show $ M.size $ children game))
                drawTextUL 2 ("Physics (ms): " ++ (show renderAvg))

                renderCairo game

            time1 <- getCurrentTime

            (pure mvFps) >>= takeMVar >>= markFrame 
                         >>= (putMVar mvFps)

            if (0.03 < diffUTCTime time1 time0) 
                then putStrLn $ "## RENDER ## Total time to get game mvar: " ++ (show $ diffUTCTime time1 time0)
                else return ()


    -- Basic control key bindings
    let onKey isPressed = do
        key <- eventKeyName
        liftIO $ do 
            gameControls <- getGameState
            let control = defaultControl
            case key of 
                "Up" -> (onUp control) gameControls isPressed >>= putGameState
                "Down" -> (onDown control) gameControls isPressed >>= putGameState
                "Left" -> (onLeft control) gameControls isPressed >>= putGameState
                "Right" -> (onRight control) gameControls isPressed >>= putGameState
                "space" -> (onFire control) gameControls isPressed >>= putGameState
                _ -> putGameState gameControls

    mainWindow `on` keyPressEvent $ tryEvent $ do onKey True
    mainWindow `on` keyReleaseEvent $ tryEvent $ do onKey False


    windowSetTitle mainWindow "Asteroids"
    windowSetDefaultSize mainWindow 400 400
    _ <- on mainWindow objectDestroy mainQuit
    containerAdd mainWindow layout
    widgetShowAll mainWindow
    resume
    mainGUI

showTime = getCurrentTime >>= putStrLn . show

about = do
    ad <- aboutDialogNew
    aboutDialogSetName ad "GTK Experiment"
    aboutDialogSetVersion ad "0.0"
    aboutDialogSetAuthors ad ["Orion Jankowski"]
    _ <- dialogRun ad
    widgetDestroy ad


background = do
    newPath
    setSourceRGB 0 0 0
    rectangle 0 0 1 1 >> fill
    closePath


eventWindowSize = do
    dr <- eventWindow
    (w, h) <- liftIO $ drawableGetSize dr
    return $ if w*h > 1
        then (fromIntegral w, fromIntegral h)
        else (1, 1)
    
getAndSet :: a -> IO (IO a, a -> IO (a))
getAndSet a = do
    mvr <- newMVar a
    let get = readMVar mvr
    let set = swapMVar mvr
    return (get,set)


data Fps = Fps [UTCTime] Int
markFrame f@(Fps times max) = do
    time0 <- getCurrentTime
    case (length times) of
            0 -> return $ Fps (time0:[]) max
            n -> if n == max 
                 then return $ Fps (time0:(take (max - 1) times)) max
                 else return $ Fps (time0:times) max

data BoxAve = BoxAve [Double] Int
addAvg v (BoxAve vs max)
    | length vs == 0 = BoxAve (v:[]) max
    | length vs == max = BoxAve (v:(take (max - 1) vs)) max
    | otherwise = BoxAve (v:vs) max

instance Show BoxAve where
    show (BoxAve vs max) = (printf "%.2f" aveVal) :: String where
        aveVal = (sum vs) / (fromIntegral max)



drawTextUL :: Double -> String -> Render()
drawTextUL pos s = do
    save
    newPath
    setSourceRGB 1 0 0
    setFontSize 0.03
    translate 0.01  (0.03 + (pos * 0.03))
    showText s
    closePath
    restore



instance Show Fps where
    show (Fps [] max) = "undefined"
    show (Fps (t:[]) max) = "undefined"
    show (Fps (t:ts) max) = (printf "%.2f fps" fps) :: String
        where diff :: Integer = fromIntegral $ unsafeCoerce (diffUTCTime t $ last ts)
              fps :: Double = 1e12 / (fromIntegral (diff `div` (fromIntegral $ length (t:ts))))
    
    

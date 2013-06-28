module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TChan, atomically, newTChanIO, tryReadTChan, writeTChan)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify)
import Data.Maybe                (catMaybes)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import Sprite (makeDisplayListFromImage)

--------------------------------------------------------------------------------

data Env = Env
    { envEventsChan    :: TChan Event
    , envWindow        :: !GLFW.Window
    , envMegaMan       :: !GL.DisplayList
    , envZDistClosest  :: !Double
    , envZDistFarthest :: !Double
    }

data State = State
    { stateWindowWidth      :: !Int
    , stateWindowHeight     :: !Int
    , stateIsCursorInWindow :: !Bool
    , stateViewXAngle       :: !Double
    , stateViewYAngle       :: !Double
    , stateViewZAngle       :: !Double
    , stateZDist            :: !Double
    }

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------

data Event =
    EventError      !GLFW.Error !String
  | EventWindowSize !GLFW.Window !Int !Int
  | EventScroll     !GLFW.Window !Double !Double
  | EventKey        !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  deriving Show

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 640
        height = 480

    eventsChan <- newTChanIO :: IO (TChan Event)

    withWindow width height "Mega Man" $ \win -> do
        GLFW.setErrorCallback          $ Just $ errorCallback      eventsChan
        GLFW.setWindowSizeCallback win $ Just $ windowSizeCallback eventsChan
        GLFW.setScrollCallback     win $ Just $ scrollCallback     eventsChan
        GLFW.setKeyCallback        win $ Just $ keyCallback        eventsChan

        GLFW.swapInterval 1

        megaMan <- makeDisplayListFromImage "megaman.png" 1

        GL.position (GL.Light 0) GL.$= GL.Vertex4 0 1 1 0
        GL.light    (GL.Light 0) GL.$= GL.Enabled
        GL.lighting   GL.$= GL.Enabled
        GL.depthFunc  GL.$= Nothing -- Just GL.Less
        GL.clearColor GL.$= GL.Color4 0.1 0.1 0.1 1
        GL.normalize  GL.$= GL.Enabled

        let zDistClosest  = 0
            zDistFarthest = 1
            zDist         = 0.2 --zDistClosest + (zDistFarthest - zDistClosest) / 2
            env = Env
              { envEventsChan    = eventsChan
              , envWindow        = win
              , envMegaMan       = megaMan
              , envZDistClosest  = zDistClosest
              , envZDistFarthest = zDistFarthest
              }
            state = State
              { stateWindowWidth      = width
              , stateWindowHeight     = height
              , stateIsCursorInWindow = False
              , stateViewXAngle       = 0
              , stateViewYAngle       = 0
              , stateViewZAngle       = 0
              , stateZDist            = zDist
              }
        runDemo env state

--------------------------------------------------------------------------------

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

errorCallback      :: TChan Event -> GLFW.Error -> String                                                            -> IO ()
windowSizeCallback :: TChan Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
scrollCallback     :: TChan Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback        :: TChan Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()

errorCallback      tc e s            = atomically $ writeTChan tc $ EventError      e s
windowSizeCallback tc win w h        = atomically $ writeTChan tc $ EventWindowSize win w h
scrollCallback     tc win x y        = atomically $ writeTChan tc $ EventScroll     win x y
keyCallback        tc win k sc ka mk = atomically $ writeTChan tc $ EventKey        win k sc ka mk

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state =
    void $ evalRWST (adjustWindow >> run) env state

run :: Demo ()
run = do
    win <- asks envWindow

    draw
    liftIO $ do
        GLFW.swapBuffers win
        GLFW.pollEvents
    processEvents

    state <- get

    (kxrot, kyrot) <- liftIO $ getCursorKeyDirections win
    (jxrot, jyrot) <- liftIO $ getJoystickDirections GLFW.Joystick'1
    (mxrot, myrot) <- if stateIsCursorInWindow state
                        then let w = stateWindowWidth  state
                                 h = stateWindowHeight state
                             in liftIO $ getMouseDirections win w h
                        else return (0, 0)

    let xa = stateViewXAngle state
        ya = stateViewYAngle state
        xa' = xa + kxrot + jxrot + mxrot
        ya' = ya + kyrot + jyrot + myrot

    modify $ \s -> s
      { stateViewXAngle = xa'
      , stateViewYAngle = ya'
      }

    q <- liftIO $ GLFW.windowShouldClose win
    unless q run

processEvents :: Demo ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTChan tc
    case me of
      (Just e) -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          printEvent "error" [show e, show s]
          win <- asks envWindow
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowSize _ width height) -> do
          printEvent "window size" [show width, show height]
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventScroll _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          printEvent "scroll" [show x', show y']
          env <- ask
          let zDistClosest  = envZDistClosest  env
              zDistFarthest = envZDistFarthest env
              inc = (zDistFarthest - zDistClosest) / 100
          modify $ \s -> s
            { stateZDist =
                let zDist' = stateZDist s + realToFrac (negate y * inc)
                in curb zDistClosest zDistFarthest zDist'
            }
          adjustWindow

      (EventKey win k scancode ks mk) -> do
          printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
          when (ks == GLFW.KeyState'Pressed) $
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True

adjustWindow :: Demo ()
adjustWindow = do
    state <- get
    let width  = stateWindowWidth  state
        height = stateWindowHeight state
        zdist  = stateZDist        state

    let pos  = GL.Position 0 0
        size = GL.Size (fromIntegral width) (fromIntegral height)
    liftIO $ do
        GL.viewport GL.$= (pos, size)

        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        let wd2 = realToFrac width  / 2
            hd2 = realToFrac height / 2
            l = negate $ wd2
            r =          wd2
            b = negate $ hd2
            t =          hd2
            c = min l b
            f = max r t
        GL.ortho l r b t c f

        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity
        GL.translate (GL.Vector3 0 0 (negate $ realToFrac zdist) :: GL.Vector3 GL.GLfloat)

draw :: Demo ()
draw = do
    env   <- ask
    state <- get
    let megaMan = envMegaMan env
        xa = stateViewXAngle state
        ya = stateViewYAngle state
        za = stateViewZAngle state
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.translate origin
            GL.scale 16 16 (1 :: GL.GLfloat)
            GL.rotate (realToFrac xa) xunit
            GL.rotate (realToFrac ya) yunit
            GL.rotate (realToFrac za) zunit
            GL.callList megaMan
      where
        origin = GL.Vector3 0 0 0 :: GL.Vector3 GL.GLfloat
        xunit  = GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat
        yunit  = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat
        zunit  = GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
    let x0n = if x0 then   2  else 0
        x1n = if x1 then (-2) else 0
        y0n = if y0 then   2  else 0
        y1n = if y1 then (-2) else 0
    return (x0n + x1n, y0n + y1n)

getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
getJoystickDirections js = do
    maxes <- GLFW.getJoystickAxes js
    return $ case maxes of
      (Just (x:y:_)) -> (y, x)
      _              -> (0, 0)

getMouseDirections :: GLFW.Window -> Int -> Int -> IO (Double, Double)
getMouseDirections win w h = do
    (x, y) <- GLFW.getCursorPos win
    let wd2 = realToFrac w / 2
        hd2 = realToFrac h / 2
        yrot = (x - wd2) / wd2
        xrot = (hd2 - y) / hd2
    return (realToFrac xrot, realToFrac yrot)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

--------------------------------------------------------------------------------

printEvent :: String -> [String] -> Demo ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

curb :: Ord a => a -> a -> a -> a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x

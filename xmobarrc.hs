module Main (main) where

import Xmobar

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

-- config is currently combo of template string and screen id

data Preset = Main | Alt
    deriving (Show, Read, Eq, Enum)

data Options =
  Options
    { optPreset   :: Preset
    , optScreen   :: Int
    , optTop      :: Bool
    , optBottom   :: Bool
    , optAlpha    :: Int
    }
    deriving (Show, Read)



makeMainTemplate, makeAltTemplate :: Int -> String
makeMainTemplate sc = let log   = "%_XMONAD_LOG_" ++ show sc ++ "%"
                          lIcon = "<icon=arch-linux-2.xpm/>"
                          rIcon = ""
                       in lIcon ++ " " ++ log ++ " }{\
                           \| %arch% CPU: %cpu% %cpufreq% %multicoretemp% \
                           \| %memory% %swap% | %wlp8s0% | %clock% %uptime% \
                           \| %date% | %time% " ++ rIcon

makeAltTemplate sc = let log   = "%_XMONAD_LOG_" ++ show sc ++ "%"
                         lIcon = "<icon=arch-linux-2.xpm/>"
                         rIcon = ""
                      in lIcon ++ " " ++ log ++ " }{\
                          \| %bell% %updates% \
                          \| %KMDW% | %time% " ++ rIcon

config :: Config
config =
  defaultConfig
    { overrideRedirect = False
    , border           = NoBorder
    , alpha            = 255      -- complete opacity
    , bgColor          = "#000000"
    , fgColor          = "#646464"
    , borderColor      = "#F8F8F2"
    , iconRoot         = "/home/carterlevo/.config/xmonad/xpm"
    , font             = makeBoldFont "Anonymous Pro" 14
    , additionalFonts  = [ makeFont "Symbols Nerd Font" 14 ]
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['v'] ["version"]
        (NoArg (\_ -> exitWithMsg "Version 0.01"))
        "print the version number"
    , Option ['l'] ["list"]
        (NoArg (\_ -> exitWithMsg . unlines $ templates))
        "list available preconfigured templates"
    , Option ['h'] ["help"]
        (NoArg (\_ -> usageInfoWithProgName >>= exitWithMsg))
        "displays this help message"
    , Option ['B'] ["bot"]
        (NoArg (\opts -> return opts { optBottom = True}))
        "place status bar on bottom of screen"
    , Option ['T'] ["top"]
        (NoArg (\opts -> return opts { optTop = True}))
        "place status bar on top of screen"
    , Option ['c'] ["config"]
        (ReqArg (\arg opts -> return opts { optPreset = read arg }) "CONFIG")
        "select preconfigured template"
    , Option ['a'] ["alpha"]
        (ReqArg (\arg opts -> return opts { optAlpha = read arg }) "OPACITY")
        "select the opacity of the bar from [0,255]"
    , Option ['x'] ["screen"]
        (ReqArg (\arg opts -> return opts { optScreen = read arg }) "SCREEN")
        "select the X screen to display status bar on"
    ]
    where
        templates :: [String]
        templates =
          [ "The available preconfigured template strings are: "
          , "1. 'Main'"
          , "2. 'Alt'"
          ]

defaultOptions :: Options
defaultOptions =
  Options
    { optPreset   = Main
    , optScreen   = 0
    , optAlpha    = 255
    , optTop      = True
    , optBottom   = False
    }

exitWithMsg :: String -> IO x
exitWithMsg s = hPutStrLn stderr s >> exitSuccess

exitWithUsage = usageInfoWithProgName >>= exitWithMsg

usageInfoWithProgName :: IO String
usageInfoWithProgName = do
    prg <- getProgName
    return (usageInfo prg options)


parseArgs = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    -- opts <- foldl (>>=) (return defaultOptions) actions
    return (foldl (>>=) (return defaultOptions) actions
    -- let screen = optScreen opts
    --     preset = optPreset opts
    -- return config { position = getXPosition screen True
    --               , commands = baseCmds ++ logCmd screen
    --               , template = getTemplateStr preset screen
    --               }

main = parseArgs >>= xmobar

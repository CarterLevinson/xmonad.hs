module Config.Static
    ( modm
    , alt
    , shift
    , ctrl
    , noMod
    , modShift
    , modCtrl
    , modAlt
    , altShift
    , ctrlAlt
    , black
    , white
    , purple
    , gray
    , term
    , shell
    , browser
    , editor
    , home
    , fallbackFont
    , anonymousPro
    , sourceCodePro
    , firaCode
    , firaSans
    , inputMono
    , inputSerif
    , hackFont
    , cliCells
    , guiCells
    , bmCells
    , dfCells
    , searchCells
    , f86AudioRaise
    , f86AudioLower
    , f86AudioNext
    , f86AudioPrev
    , f86AudioPlay
    , f86AudioPause
    , f86AudioMute
    , f86AudioMicMute
    ) where

import Config.Data

import Data.Bits ((.|.))
import Graphics.X11
import Graphics.X11.ExtraTypes.XF86

-- static data
modm, alt, shift, ctrl, noMod :: KeyMask
modm     = mod4Mask
alt      = mod1Mask
shift    = shiftMask
ctrl     = controlMask
noMod    = noModMask

modShift, modCtrl, modAlt :: KeyMask
modShift = modm .|. shift
modCtrl  = modm .|. ctrl
modAlt   = modm .|. alt

altShift, ctrlAlt :: KeyMask
altShift = alt  .|. shift
ctrlAlt  = alt  .|. ctrl

black, white, purple, gray :: HexColorCode
black  = "#000000"
white  = "#FFFFFF"
purple = "#A865C9"
gray   = "#DDDDDD"

f86AudioNext, f86AudioPrev, f86AudioPlay, f86AudioPause :: KeySym
f86AudioNext    = xF86XK_AudioNext
f86AudioPrev    = xF86XK_AudioPrev
f86AudioPlay    = xF86XK_AudioPlay
f86AudioPause   = xF86XK_AudioPause

f86AudioMute, f86AudioRaise, f86AudioLower, f86AudioMicMute :: KeySym
f86AudioMute    = xF86XK_AudioMute
f86AudioRaise   = xF86XK_AudioRaiseVolume
f86AudioLower   = xF86XK_AudioLowerVolume
f86AudioMicMute = xF86XK_AudioMicMute

term, browser, editor, shell, hledger :: Command
term     = "kitty"
browser  = "firefox"
editor   = "nvim"
shell    = "zsh"
-- do ledger env file
hledger  = "hledger-ui -f ~/finance/.hledger.journal"

-- home, anonPro :: String
home :: Path
home     = "/home/carterlevo"

fallbackFont    :: XftFont
fallbackFont    = XFT
    { family    = "DejaVu Sans Mono"
    , size      = 14.0
    , weight    = Bold
    , slant     = Roman
    , spacing   = Mono
    , hinting   = True
    , antialias = True
    , pixelsize = Nothing
    }

hackFont :: XftFont
hackFont        = fallbackFont
    { family    = "Hack"    }

firaCode :: XftFont
firaCode        = fallbackFont
    { family    = "Fira Code" }

firaSans :: XftFont
firaSans        = fallbackFont
    { family    = "Fira Sans" }

inputMono :: XftFont
inputMono       = fallbackFont
    { family    = "Input Mono" }

inputSerif :: XftFont
inputSerif      = fallbackFont
    { family    = "Input Serif" }

anonymousPro :: XftFont
anonymousPro    = fallbackFont
    { family    = "Anonymous Pro" }

sourceCodePro :: XftFont
sourceCodePro   = fallbackFont
    { family    = "Source Code Pro" }



cliCells, guiCells, bmCells, dfCells :: [Cell]
cliCells =
    [ ("Ipython", "ipython")
    , ("GHCi", "ghci")
    , ("Radian", "radian")
    , ("GiNsh", "ginsh")
    , ("Basic Calculator", "bc -q")
    , ("Maxima", "maxima")
    , ("Cmus", "cmus")
    , ("Calcurse", "calcurse")
    , ("GCal", "gcalcli")
    , ("FriCAS", "fricas")
    , ("Gap", "gap")
    , ("Sc-im", "sc-im")
    , ("Neomutt", "neomutt")
    , ("VisiData", "vd")
    , ("WeeChat", "weechat")
    , ("Gnuplot", "gnuplot")
    , ("Htop", "htop")
    , ("Btop++", "btop")
    , ("S-tui", "s-tui")
    , ("Termshark", "termshark")
    , ("Ncdu", "ncdu")
    , ("Cfdisk", "cfdisk")
    , ("Lvm Manager", "lvm")
    , ("Ranger", "ranger")
    , ("Ledger", "ledger")
    , ("Hledger", hledger)
    ]
guiCells =
    [ ("Vimiv", "vimiv")
    , ("Spotify", "spotify-launcer")
    , ("Gimp", "gimp")
    , ("wxMaxima", "wxmaxima")
    , ("Xasy", "xasy")
    , ("Inkscape", "inkscape")
    , ("Trader Workstation", "tws")
    , ("TradingView", "tradingview")
    ]
dfCells =
    [ ("Config", "~/.config")
    , ("Neovim", "~/.config/nvim")
    , ("Kitty", "~/.config/kitty")
    , ("Ranger", "~/.config/ranger")
    , ("Bash", "~/.bashrc")
    , ("Git", "~/.gitconfig")
    , ("XMonad", "~/.xmonad")
    , ("Xmobar", "~/.xmobar")
    , ("KMonad", "~/.config/kmonad")
    , ("Firefox", "~/.mozilla")
    , ("Zathura", "~/.config/zathura/zathurarc")
    , ("Qutebrowser", "~/.config/qutebrowser")
    , ("Zsh", "~/.zshrc")
    ]
bmCells =
    [ ("Gmail", "mail.google.com")
    , ("Posteo", "posteo.de/en")
    , ("LessWrong", "lesswrong.com")
    , ("Leetcode", "leetcode.com")
    , ("Python Documentation", "docs.python.com")
    , ("Haskell Wiki", "wiki.haskell.org")
    , ("Vim cheatsheet", "vim.rotrr.com")
    , ("Neovim Documentation", "neovio.io/doc")
    , ("Git Documentation", "git-scm.com/doc")
    , ("Arch Wiki", "wiki.archlinux.org")
    , ("Linux manpages", "linux.die.net/man")
    , ("Wikichip", "wikichip.org")
    , ("Libgen", "libgen.is")
    , ("Sci-hub", "sc-hubtw.hkvisa.net")
    , ("Wikibooks", "wikibooks.org")
    , ("Open Library", "openlibrary.org")
    , ("arXiv", "arxiv.org")
    , ("SSRN", "ssrn.com")
    , ("JSTOR", "jstor.org")
    , ("Lexis Nexis", "lexisnexis.org")
    , ("ACM Library", "dl.acm.org")
    , ("CIA World Factbook", "cia.gov/the-world-factbook")
    , ("Project Euler", "projecteuler.net")
    , ("Wolfram Mathworld", "mathworld.wolfram.com")
    ]

searchCells :: [Cell]
searchCells =
    [ ( "", "" )
    ]

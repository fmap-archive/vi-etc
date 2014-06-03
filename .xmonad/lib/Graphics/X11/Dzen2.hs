{-# LANGUAGE FlexibleInstances #-}

module Graphics.X11.Dzen2 (
  Dzen2Config(..), 
  Timeout(..),
  Alignment(..),
  dzen
) where

import Data.Monoid (mconcat)
import Data.List (intercalate)

type Timeout = Int

data Alignment = LeftA | CentreA | RightA

instance Show Alignment where
  show LeftA   = "l"
  show CentreA = "c"
  show RightA  = "r"

data Dzen2Config = Dzen2Config 
  { p  :: Either () (Maybe Timeout)
  , fg :: Maybe String
  , bg :: Maybe String
  , fn :: Maybe String
  , ta :: Maybe Alignment
  , tw :: Maybe Int
  , sa :: Maybe Alignment
  , l  :: Maybe Int
  , e  :: Maybe [Event]
  , m  :: Either () (Maybe MenuMode)
  , u  :: Bool
  , x  :: Maybe Int
  , y  :: Maybe Int
  , h  :: Maybe Int
  , w  :: Maybe Int
  , xs :: Maybe Int
  }

data MenuMode = Vertical | Horizontal

instance Show MenuMode where
  show Vertical = "v"
  show Horizontal = "h"

showMenu :: Either () (Maybe MenuMode) -> Maybe String
showMenu (Left ()) = Nothing
showMenu (Right Nothing) = Just ""
showMenu (Right a) = Just $ show a

showPersist :: Either () (Maybe Timeout) -> Maybe String
showPersist (Left ())        = Nothing
showPersist (Right Nothing)  = Just ""
showPersist (Right (Just i)) = Just $ show i

data Event = Event
  { eventName :: String
  , actions   :: [Action]
  }

data Action = Action
  { actionName :: String
  , options    :: [Option]
  }

type Option = String

instance Show Action where
  show action = intercalate ":" $
    actionName action : options action

instance Show Event where
  show event = mconcat $ 
    [ eventName event
    , "="
    , intercalate "," $ map show $ actions event
    ]
  showList events = const $ intercalate ";" $ map show events

-- [Event "button1" [Action "exec" ["xterm","firefox"]], Event "entertitle" [Action "uncollapse" [], Action "unhide" []]]

showSimultaneousUpdates :: Bool -> Maybe String
showSimultaneousUpdates x | x = Just ""
                          | otherwise = Nothing

dzen2Config :: Dzen2Config -> String
dzen2Config cfg = mconcat . ("dzen2":) $
  [ dzenOption ("-p",  showPersist $ p cfg)
  , dzenOption ("-fg", innerShow $ fg cfg)
  , dzenOption ("-bg", innerShow $ bg cfg)
  , dzenOption ("-fn", innerShow $ fn cfg)
  , dzenOption ("-ta", innerShow $ ta cfg)
  , dzenOption ("-tw", innerShow $ tw cfg)
  , dzenOption ("-sa", innerShow $ sa cfg)
  , dzenOption ("-l",  innerShow $ l cfg)
  , dzenOption ("-e",  innerShow $ e cfg)
  , dzenOption ("-m",  showMenu $ m cfg)
  , dzenOption ("-u",  showSimultaneousUpdates $ u cfg)
  , dzenOption ("-x", innerShow $ x cfg)
  , dzenOption ("-y", innerShow $ y cfg)
  , dzenOption ("-h", innerShow $ h cfg)
  , dzenOption ("-w", innerShow $ w cfg)
  , dzenOption ("-xs", innerShow $ xs cfg)
  ] where 
    innerShow :: Show a => Maybe a -> Maybe String
    innerShow = fmap show

emptyDzen2Config :: Dzen2Config
emptyDzen2Config = Dzen2Config
  { p  = Left ()
  , fg = Nothing
  , bg = Nothing
  , fn = Nothing
  , ta = Nothing
  , tw = Nothing
  , sa = Nothing
  , l  = Nothing
  , e  = Nothing
  , m  = Left ()
  , u  = False
  , x  = Nothing
  , y  = Nothing
  , h  = Nothing
  , w  = Nothing
  , xs = Nothing
  }

instance Show Dzen2Config where
  show = dzen2Config

space :: String
space = " "

dzenOption :: (String, Maybe String) -> String
dzenOption (_, Nothing)     = ""
dzenOption (flag, Just arg) = mconcat [space, flag, space, arg]

dzen :: Dzen2Config
dzen = emptyDzen2Config

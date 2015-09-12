module Mpris.Utils
       ( unpack
       ) where

import DBus
import Data.Maybe (fromMaybe)

unpack :: IsVariant a => Variant -> a
unpack var = fromMaybe (error $ "Could not unpack variant: " ++ show var) $ fromVariant var

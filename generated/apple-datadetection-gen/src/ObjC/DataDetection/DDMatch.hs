{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A base class for common types of data that the data detection system matches.
--
-- The DataDetection framework returns results in objects that are subclasses of @DDMatch@, which are specific to the type of matching data. Each object contains the matched string.
--
-- Generated bindings for @DDMatch@.
module ObjC.DataDetection.DDMatch
  ( DDMatch
  , IsDDMatch(..)
  , init_
  , matchedString
  , initSelector
  , matchedStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsDDMatch ddMatch => ddMatch -> IO (Id DDMatch)
init_ ddMatch =
  sendOwnedMessage ddMatch initSelector

-- | A substring that the data detection system identifies from an original string as a common type of data.
--
-- Use @DDMatch@ subclasses that the data detection system provides for a semantic interpretation of this string.
--
-- ObjC selector: @- matchedString@
matchedString :: IsDDMatch ddMatch => ddMatch -> IO (Id NSString)
matchedString ddMatch =
  sendMessage ddMatch matchedStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id DDMatch)
initSelector = mkSelector "init"

-- | @Selector@ for @matchedString@
matchedStringSelector :: Selector '[] (Id NSString)
matchedStringSelector = mkSelector "matchedString"


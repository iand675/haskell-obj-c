{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that specifies how to filter a property against a given string and comparison options.
--
-- Generated bindings for @ASPropertyCompareString@.
module ObjC.AccessorySetupKit.ASPropertyCompareString
  ( ASPropertyCompareString
  , IsASPropertyCompareString(..)
  , initWithString_compareOptions
  , init_
  , new
  , string
  , compareOptions
  , compareOptionsSelector
  , initSelector
  , initWithString_compareOptionsSelector
  , newSelector
  , stringSelector

  -- * Enum types
  , NSStringCompareOptions(NSStringCompareOptions)
  , pattern NSCaseInsensitiveSearch
  , pattern NSLiteralSearch
  , pattern NSBackwardsSearch
  , pattern NSAnchoredSearch
  , pattern NSNumericSearch
  , pattern NSDiacriticInsensitiveSearch
  , pattern NSWidthInsensitiveSearch
  , pattern NSForcedOrderingSearch
  , pattern NSRegularExpressionSearch

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a property compare string instance with the given string and comparison options. - Parameters:   - string: The string to compare against.   - compareOptions: Options to apply when comparing strings.
--
-- ObjC selector: @- initWithString:compareOptions:@
initWithString_compareOptions :: (IsASPropertyCompareString asPropertyCompareString, IsNSString string) => asPropertyCompareString -> string -> NSStringCompareOptions -> IO (Id ASPropertyCompareString)
initWithString_compareOptions asPropertyCompareString string compareOptions =
  sendOwnedMessage asPropertyCompareString initWithString_compareOptionsSelector (toNSString string) compareOptions

-- | @- init@
init_ :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO (Id ASPropertyCompareString)
init_ asPropertyCompareString =
  sendOwnedMessage asPropertyCompareString initSelector

-- | @- new@
new :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO (Id ASPropertyCompareString)
new asPropertyCompareString =
  sendOwnedMessage asPropertyCompareString newSelector

-- | The string to compare against.
--
-- ObjC selector: @- string@
string :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO (Id NSString)
string asPropertyCompareString =
  sendMessage asPropertyCompareString stringSelector

-- | Comparison options to apply when comparing strings.
--
-- ObjC selector: @- compareOptions@
compareOptions :: IsASPropertyCompareString asPropertyCompareString => asPropertyCompareString -> IO NSStringCompareOptions
compareOptions asPropertyCompareString =
  sendMessage asPropertyCompareString compareOptionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithString:compareOptions:@
initWithString_compareOptionsSelector :: Selector '[Id NSString, NSStringCompareOptions] (Id ASPropertyCompareString)
initWithString_compareOptionsSelector = mkSelector "initWithString:compareOptions:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPropertyCompareString)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASPropertyCompareString)
newSelector = mkSelector "new"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @compareOptions@
compareOptionsSelector :: Selector '[] NSStringCompareOptions
compareOptionsSelector = mkSelector "compareOptions"


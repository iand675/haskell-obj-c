{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextAlternatives@.
module ObjC.AppKit.NSTextAlternatives
  ( NSTextAlternatives
  , IsNSTextAlternatives(..)
  , initWithPrimaryString_alternativeStrings
  , noteSelectedAlternativeString
  , primaryString
  , alternativeStrings
  , alternativeStringsSelector
  , initWithPrimaryString_alternativeStringsSelector
  , noteSelectedAlternativeStringSelector
  , primaryStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPrimaryString:alternativeStrings:@
initWithPrimaryString_alternativeStrings :: (IsNSTextAlternatives nsTextAlternatives, IsNSString primaryString, IsNSArray alternativeStrings) => nsTextAlternatives -> primaryString -> alternativeStrings -> IO (Id NSTextAlternatives)
initWithPrimaryString_alternativeStrings nsTextAlternatives primaryString alternativeStrings =
  sendOwnedMessage nsTextAlternatives initWithPrimaryString_alternativeStringsSelector (toNSString primaryString) (toNSArray alternativeStrings)

-- | @- noteSelectedAlternativeString:@
noteSelectedAlternativeString :: (IsNSTextAlternatives nsTextAlternatives, IsNSString alternativeString) => nsTextAlternatives -> alternativeString -> IO ()
noteSelectedAlternativeString nsTextAlternatives alternativeString =
  sendMessage nsTextAlternatives noteSelectedAlternativeStringSelector (toNSString alternativeString)

-- | @- primaryString@
primaryString :: IsNSTextAlternatives nsTextAlternatives => nsTextAlternatives -> IO (Id NSString)
primaryString nsTextAlternatives =
  sendMessage nsTextAlternatives primaryStringSelector

-- | @- alternativeStrings@
alternativeStrings :: IsNSTextAlternatives nsTextAlternatives => nsTextAlternatives -> IO (Id NSArray)
alternativeStrings nsTextAlternatives =
  sendMessage nsTextAlternatives alternativeStringsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPrimaryString:alternativeStrings:@
initWithPrimaryString_alternativeStringsSelector :: Selector '[Id NSString, Id NSArray] (Id NSTextAlternatives)
initWithPrimaryString_alternativeStringsSelector = mkSelector "initWithPrimaryString:alternativeStrings:"

-- | @Selector@ for @noteSelectedAlternativeString:@
noteSelectedAlternativeStringSelector :: Selector '[Id NSString] ()
noteSelectedAlternativeStringSelector = mkSelector "noteSelectedAlternativeString:"

-- | @Selector@ for @primaryString@
primaryStringSelector :: Selector '[] (Id NSString)
primaryStringSelector = mkSelector "primaryString"

-- | @Selector@ for @alternativeStrings@
alternativeStringsSelector :: Selector '[] (Id NSArray)
alternativeStringsSelector = mkSelector "alternativeStrings"


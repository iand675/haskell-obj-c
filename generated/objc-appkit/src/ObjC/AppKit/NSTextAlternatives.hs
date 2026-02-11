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
  , initWithPrimaryString_alternativeStringsSelector
  , noteSelectedAlternativeStringSelector
  , primaryStringSelector
  , alternativeStringsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPrimaryString:alternativeStrings:@
initWithPrimaryString_alternativeStrings :: (IsNSTextAlternatives nsTextAlternatives, IsNSString primaryString, IsNSArray alternativeStrings) => nsTextAlternatives -> primaryString -> alternativeStrings -> IO (Id NSTextAlternatives)
initWithPrimaryString_alternativeStrings nsTextAlternatives  primaryString alternativeStrings =
withObjCPtr primaryString $ \raw_primaryString ->
  withObjCPtr alternativeStrings $ \raw_alternativeStrings ->
      sendMsg nsTextAlternatives (mkSelector "initWithPrimaryString:alternativeStrings:") (retPtr retVoid) [argPtr (castPtr raw_primaryString :: Ptr ()), argPtr (castPtr raw_alternativeStrings :: Ptr ())] >>= ownedObject . castPtr

-- | @- noteSelectedAlternativeString:@
noteSelectedAlternativeString :: (IsNSTextAlternatives nsTextAlternatives, IsNSString alternativeString) => nsTextAlternatives -> alternativeString -> IO ()
noteSelectedAlternativeString nsTextAlternatives  alternativeString =
withObjCPtr alternativeString $ \raw_alternativeString ->
    sendMsg nsTextAlternatives (mkSelector "noteSelectedAlternativeString:") retVoid [argPtr (castPtr raw_alternativeString :: Ptr ())]

-- | @- primaryString@
primaryString :: IsNSTextAlternatives nsTextAlternatives => nsTextAlternatives -> IO (Id NSString)
primaryString nsTextAlternatives  =
  sendMsg nsTextAlternatives (mkSelector "primaryString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alternativeStrings@
alternativeStrings :: IsNSTextAlternatives nsTextAlternatives => nsTextAlternatives -> IO (Id NSArray)
alternativeStrings nsTextAlternatives  =
  sendMsg nsTextAlternatives (mkSelector "alternativeStrings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPrimaryString:alternativeStrings:@
initWithPrimaryString_alternativeStringsSelector :: Selector
initWithPrimaryString_alternativeStringsSelector = mkSelector "initWithPrimaryString:alternativeStrings:"

-- | @Selector@ for @noteSelectedAlternativeString:@
noteSelectedAlternativeStringSelector :: Selector
noteSelectedAlternativeStringSelector = mkSelector "noteSelectedAlternativeString:"

-- | @Selector@ for @primaryString@
primaryStringSelector :: Selector
primaryStringSelector = mkSelector "primaryString"

-- | @Selector@ for @alternativeStrings@
alternativeStringsSelector :: Selector
alternativeStringsSelector = mkSelector "alternativeStrings"


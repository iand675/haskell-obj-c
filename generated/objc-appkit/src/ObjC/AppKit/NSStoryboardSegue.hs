{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSStoryboardSegue@.
module ObjC.AppKit.NSStoryboardSegue
  ( NSStoryboardSegue
  , IsNSStoryboardSegue(..)
  , segueWithIdentifier_source_destination_performHandler
  , initWithIdentifier_source_destination
  , perform
  , identifier
  , sourceController
  , destinationController
  , segueWithIdentifier_source_destination_performHandlerSelector
  , initWithIdentifier_source_destinationSelector
  , performSelector
  , identifierSelector
  , sourceControllerSelector
  , destinationControllerSelector


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

-- | @+ segueWithIdentifier:source:destination:performHandler:@
segueWithIdentifier_source_destination_performHandler :: IsNSString identifier => identifier -> RawId -> RawId -> Ptr () -> IO (Id NSStoryboardSegue)
segueWithIdentifier_source_destination_performHandler identifier sourceController destinationController performHandler =
  do
    cls' <- getRequiredClass "NSStoryboardSegue"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "segueWithIdentifier:source:destination:performHandler:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr (unRawId sourceController) :: Ptr ()), argPtr (castPtr (unRawId destinationController) :: Ptr ()), argPtr (castPtr performHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithIdentifier:source:destination:@
initWithIdentifier_source_destination :: (IsNSStoryboardSegue nsStoryboardSegue, IsNSString identifier) => nsStoryboardSegue -> identifier -> RawId -> RawId -> IO (Id NSStoryboardSegue)
initWithIdentifier_source_destination nsStoryboardSegue  identifier sourceController destinationController =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsStoryboardSegue (mkSelector "initWithIdentifier:source:destination:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr (unRawId sourceController) :: Ptr ()), argPtr (castPtr (unRawId destinationController) :: Ptr ())] >>= ownedObject . castPtr

-- | @- perform@
perform :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO ()
perform nsStoryboardSegue  =
  sendMsg nsStoryboardSegue (mkSelector "perform") retVoid []

-- | @- identifier@
identifier :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO (Id NSString)
identifier nsStoryboardSegue  =
  sendMsg nsStoryboardSegue (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceController@
sourceController :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO RawId
sourceController nsStoryboardSegue  =
  fmap (RawId . castPtr) $ sendMsg nsStoryboardSegue (mkSelector "sourceController") (retPtr retVoid) []

-- | @- destinationController@
destinationController :: IsNSStoryboardSegue nsStoryboardSegue => nsStoryboardSegue -> IO RawId
destinationController nsStoryboardSegue  =
  fmap (RawId . castPtr) $ sendMsg nsStoryboardSegue (mkSelector "destinationController") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @segueWithIdentifier:source:destination:performHandler:@
segueWithIdentifier_source_destination_performHandlerSelector :: Selector
segueWithIdentifier_source_destination_performHandlerSelector = mkSelector "segueWithIdentifier:source:destination:performHandler:"

-- | @Selector@ for @initWithIdentifier:source:destination:@
initWithIdentifier_source_destinationSelector :: Selector
initWithIdentifier_source_destinationSelector = mkSelector "initWithIdentifier:source:destination:"

-- | @Selector@ for @perform@
performSelector :: Selector
performSelector = mkSelector "perform"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @sourceController@
sourceControllerSelector :: Selector
sourceControllerSelector = mkSelector "sourceController"

-- | @Selector@ for @destinationController@
destinationControllerSelector :: Selector
destinationControllerSelector = mkSelector "destinationController"


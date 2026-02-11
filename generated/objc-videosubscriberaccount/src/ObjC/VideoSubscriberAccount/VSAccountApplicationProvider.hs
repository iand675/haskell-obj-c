{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object which provides an account provider to be added to the list of providers in your application.
--
-- Generated bindings for @VSAccountApplicationProvider@.
module ObjC.VideoSubscriberAccount.VSAccountApplicationProvider
  ( VSAccountApplicationProvider
  , IsVSAccountApplicationProvider(..)
  , init_
  , new
  , initWithLocalizedDisplayName_identifier
  , localizedDisplayName
  , identifier
  , initSelector
  , newSelector
  , initWithLocalizedDisplayName_identifierSelector
  , localizedDisplayNameSelector
  , identifierSelector


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

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable for this class.
--
-- ObjC selector: @- init@
init_ :: IsVSAccountApplicationProvider vsAccountApplicationProvider => vsAccountApplicationProvider -> IO (Id VSAccountApplicationProvider)
init_ vsAccountApplicationProvider  =
  sendMsg vsAccountApplicationProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Unavailable for this class.
--
-- ObjC selector: @+ new@
new :: IO (Id VSAccountApplicationProvider)
new  =
  do
    cls' <- getRequiredClass "VSAccountApplicationProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns an application provider using a given display name and identifier. Both the localizedDisplayName and identifier parameters must be non-empty strings.
--
-- ObjC selector: @- initWithLocalizedDisplayName:identifier:@
initWithLocalizedDisplayName_identifier :: (IsVSAccountApplicationProvider vsAccountApplicationProvider, IsNSString localizedDisplayName, IsNSString identifier) => vsAccountApplicationProvider -> localizedDisplayName -> identifier -> IO (Id VSAccountApplicationProvider)
initWithLocalizedDisplayName_identifier vsAccountApplicationProvider  localizedDisplayName identifier =
withObjCPtr localizedDisplayName $ \raw_localizedDisplayName ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg vsAccountApplicationProvider (mkSelector "initWithLocalizedDisplayName:identifier:") (retPtr retVoid) [argPtr (castPtr raw_localizedDisplayName :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | The display name of the provider as it will appear in the list of providers.
--
-- ObjC selector: @- localizedDisplayName@
localizedDisplayName :: IsVSAccountApplicationProvider vsAccountApplicationProvider => vsAccountApplicationProvider -> IO (Id NSString)
localizedDisplayName vsAccountApplicationProvider  =
  sendMsg vsAccountApplicationProvider (mkSelector "localizedDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifier of the provider. If selected, this value is returned to your application.
--
-- ObjC selector: @- identifier@
identifier :: IsVSAccountApplicationProvider vsAccountApplicationProvider => vsAccountApplicationProvider -> IO (Id NSString)
identifier vsAccountApplicationProvider  =
  sendMsg vsAccountApplicationProvider (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithLocalizedDisplayName:identifier:@
initWithLocalizedDisplayName_identifierSelector :: Selector
initWithLocalizedDisplayName_identifierSelector = mkSelector "initWithLocalizedDisplayName:identifier:"

-- | @Selector@ for @localizedDisplayName@
localizedDisplayNameSelector :: Selector
localizedDisplayNameSelector = mkSelector "localizedDisplayName"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"


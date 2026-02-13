{-# LANGUAGE DataKinds #-}
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
  , identifierSelector
  , initSelector
  , initWithLocalizedDisplayName_identifierSelector
  , localizedDisplayNameSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoSubscriberAccount.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Unavailable for this class.
--
-- ObjC selector: @- init@
init_ :: IsVSAccountApplicationProvider vsAccountApplicationProvider => vsAccountApplicationProvider -> IO (Id VSAccountApplicationProvider)
init_ vsAccountApplicationProvider =
  sendOwnedMessage vsAccountApplicationProvider initSelector

-- | Unavailable for this class.
--
-- ObjC selector: @+ new@
new :: IO (Id VSAccountApplicationProvider)
new  =
  do
    cls' <- getRequiredClass "VSAccountApplicationProvider"
    sendOwnedClassMessage cls' newSelector

-- | Returns an application provider using a given display name and identifier. Both the localizedDisplayName and identifier parameters must be non-empty strings.
--
-- ObjC selector: @- initWithLocalizedDisplayName:identifier:@
initWithLocalizedDisplayName_identifier :: (IsVSAccountApplicationProvider vsAccountApplicationProvider, IsNSString localizedDisplayName, IsNSString identifier) => vsAccountApplicationProvider -> localizedDisplayName -> identifier -> IO (Id VSAccountApplicationProvider)
initWithLocalizedDisplayName_identifier vsAccountApplicationProvider localizedDisplayName identifier =
  sendOwnedMessage vsAccountApplicationProvider initWithLocalizedDisplayName_identifierSelector (toNSString localizedDisplayName) (toNSString identifier)

-- | The display name of the provider as it will appear in the list of providers.
--
-- ObjC selector: @- localizedDisplayName@
localizedDisplayName :: IsVSAccountApplicationProvider vsAccountApplicationProvider => vsAccountApplicationProvider -> IO (Id NSString)
localizedDisplayName vsAccountApplicationProvider =
  sendMessage vsAccountApplicationProvider localizedDisplayNameSelector

-- | The identifier of the provider. If selected, this value is returned to your application.
--
-- ObjC selector: @- identifier@
identifier :: IsVSAccountApplicationProvider vsAccountApplicationProvider => vsAccountApplicationProvider -> IO (Id NSString)
identifier vsAccountApplicationProvider =
  sendMessage vsAccountApplicationProvider identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VSAccountApplicationProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VSAccountApplicationProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithLocalizedDisplayName:identifier:@
initWithLocalizedDisplayName_identifierSelector :: Selector '[Id NSString, Id NSString] (Id VSAccountApplicationProvider)
initWithLocalizedDisplayName_identifierSelector = mkSelector "initWithLocalizedDisplayName:identifier:"

-- | @Selector@ for @localizedDisplayName@
localizedDisplayNameSelector :: Selector '[] (Id NSString)
localizedDisplayNameSelector = mkSelector "localizedDisplayName"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"


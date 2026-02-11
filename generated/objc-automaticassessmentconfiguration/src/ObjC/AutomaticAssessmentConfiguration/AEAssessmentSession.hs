{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AEAssessmentSession@.
module ObjC.AutomaticAssessmentConfiguration.AEAssessmentSession
  ( AEAssessmentSession
  , IsAEAssessmentSession(..)
  , initWithConfiguration
  , init_
  , new
  , begin
  , end
  , updateToConfiguration
  , supportsMultipleParticipants
  , supportsConfigurationUpdates
  , configuration
  , active
  , initWithConfigurationSelector
  , initSelector
  , newSelector
  , beginSelector
  , endSelector
  , updateToConfigurationSelector
  , supportsMultipleParticipantsSelector
  , supportsConfigurationUpdatesSelector
  , configurationSelector
  , activeSelector


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

import ObjC.AutomaticAssessmentConfiguration.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithConfiguration:@
initWithConfiguration :: (IsAEAssessmentSession aeAssessmentSession, IsAEAssessmentConfiguration configuration) => aeAssessmentSession -> configuration -> IO (Id AEAssessmentSession)
initWithConfiguration aeAssessmentSession  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg aeAssessmentSession (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO (Id AEAssessmentSession)
init_ aeAssessmentSession  =
  sendMsg aeAssessmentSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AEAssessmentSession)
new  =
  do
    cls' <- getRequiredClass "AEAssessmentSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- begin@
begin :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO ()
begin aeAssessmentSession  =
  sendMsg aeAssessmentSession (mkSelector "begin") retVoid []

-- | @- end@
end :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO ()
end aeAssessmentSession  =
  sendMsg aeAssessmentSession (mkSelector "end") retVoid []

-- | @- updateToConfiguration:@
updateToConfiguration :: (IsAEAssessmentSession aeAssessmentSession, IsAEAssessmentConfiguration configuration) => aeAssessmentSession -> configuration -> IO ()
updateToConfiguration aeAssessmentSession  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg aeAssessmentSession (mkSelector "updateToConfiguration:") retVoid [argPtr (castPtr raw_configuration :: Ptr ())]

-- | @+ supportsMultipleParticipants@
supportsMultipleParticipants :: IO Bool
supportsMultipleParticipants  =
  do
    cls' <- getRequiredClass "AEAssessmentSession"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsMultipleParticipants") retCULong []

-- | @+ supportsConfigurationUpdates@
supportsConfigurationUpdates :: IO Bool
supportsConfigurationUpdates  =
  do
    cls' <- getRequiredClass "AEAssessmentSession"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsConfigurationUpdates") retCULong []

-- | @- configuration@
configuration :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO (Id AEAssessmentConfiguration)
configuration aeAssessmentSession  =
  sendMsg aeAssessmentSession (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- active@
active :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO Bool
active aeAssessmentSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentSession (mkSelector "active") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @begin@
beginSelector :: Selector
beginSelector = mkSelector "begin"

-- | @Selector@ for @end@
endSelector :: Selector
endSelector = mkSelector "end"

-- | @Selector@ for @updateToConfiguration:@
updateToConfigurationSelector :: Selector
updateToConfigurationSelector = mkSelector "updateToConfiguration:"

-- | @Selector@ for @supportsMultipleParticipants@
supportsMultipleParticipantsSelector :: Selector
supportsMultipleParticipantsSelector = mkSelector "supportsMultipleParticipants"

-- | @Selector@ for @supportsConfigurationUpdates@
supportsConfigurationUpdatesSelector :: Selector
supportsConfigurationUpdatesSelector = mkSelector "supportsConfigurationUpdates"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"


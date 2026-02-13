{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , configuration
  , active
  , activeSelector
  , beginSelector
  , configurationSelector
  , delegateSelector
  , endSelector
  , initSelector
  , initWithConfigurationSelector
  , newSelector
  , setDelegateSelector
  , supportsConfigurationUpdatesSelector
  , supportsMultipleParticipantsSelector
  , updateToConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AutomaticAssessmentConfiguration.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithConfiguration:@
initWithConfiguration :: (IsAEAssessmentSession aeAssessmentSession, IsAEAssessmentConfiguration configuration) => aeAssessmentSession -> configuration -> IO (Id AEAssessmentSession)
initWithConfiguration aeAssessmentSession configuration =
  sendOwnedMessage aeAssessmentSession initWithConfigurationSelector (toAEAssessmentConfiguration configuration)

-- | @- init@
init_ :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO (Id AEAssessmentSession)
init_ aeAssessmentSession =
  sendOwnedMessage aeAssessmentSession initSelector

-- | @+ new@
new :: IO (Id AEAssessmentSession)
new  =
  do
    cls' <- getRequiredClass "AEAssessmentSession"
    sendOwnedClassMessage cls' newSelector

-- | @- begin@
begin :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO ()
begin aeAssessmentSession =
  sendMessage aeAssessmentSession beginSelector

-- | @- end@
end :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO ()
end aeAssessmentSession =
  sendMessage aeAssessmentSession endSelector

-- | @- updateToConfiguration:@
updateToConfiguration :: (IsAEAssessmentSession aeAssessmentSession, IsAEAssessmentConfiguration configuration) => aeAssessmentSession -> configuration -> IO ()
updateToConfiguration aeAssessmentSession configuration =
  sendMessage aeAssessmentSession updateToConfigurationSelector (toAEAssessmentConfiguration configuration)

-- | @+ supportsMultipleParticipants@
supportsMultipleParticipants :: IO Bool
supportsMultipleParticipants  =
  do
    cls' <- getRequiredClass "AEAssessmentSession"
    sendClassMessage cls' supportsMultipleParticipantsSelector

-- | @+ supportsConfigurationUpdates@
supportsConfigurationUpdates :: IO Bool
supportsConfigurationUpdates  =
  do
    cls' <- getRequiredClass "AEAssessmentSession"
    sendClassMessage cls' supportsConfigurationUpdatesSelector

-- | @- delegate@
delegate :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO RawId
delegate aeAssessmentSession =
  sendMessage aeAssessmentSession delegateSelector

-- | @- setDelegate:@
setDelegate :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> RawId -> IO ()
setDelegate aeAssessmentSession value =
  sendMessage aeAssessmentSession setDelegateSelector value

-- | @- configuration@
configuration :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO (Id AEAssessmentConfiguration)
configuration aeAssessmentSession =
  sendMessage aeAssessmentSession configurationSelector

-- | @- active@
active :: IsAEAssessmentSession aeAssessmentSession => aeAssessmentSession -> IO Bool
active aeAssessmentSession =
  sendMessage aeAssessmentSession activeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector '[Id AEAssessmentConfiguration] (Id AEAssessmentSession)
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AEAssessmentSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AEAssessmentSession)
newSelector = mkSelector "new"

-- | @Selector@ for @begin@
beginSelector :: Selector '[] ()
beginSelector = mkSelector "begin"

-- | @Selector@ for @end@
endSelector :: Selector '[] ()
endSelector = mkSelector "end"

-- | @Selector@ for @updateToConfiguration:@
updateToConfigurationSelector :: Selector '[Id AEAssessmentConfiguration] ()
updateToConfigurationSelector = mkSelector "updateToConfiguration:"

-- | @Selector@ for @supportsMultipleParticipants@
supportsMultipleParticipantsSelector :: Selector '[] Bool
supportsMultipleParticipantsSelector = mkSelector "supportsMultipleParticipants"

-- | @Selector@ for @supportsConfigurationUpdates@
supportsConfigurationUpdatesSelector :: Selector '[] Bool
supportsConfigurationUpdatesSelector = mkSelector "supportsConfigurationUpdates"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id AEAssessmentConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"


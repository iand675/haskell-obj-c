{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AEAssessmentParticipantConfiguration@.
module ObjC.AutomaticAssessmentConfiguration.AEAssessmentParticipantConfiguration
  ( AEAssessmentParticipantConfiguration
  , IsAEAssessmentParticipantConfiguration(..)
  , init_
  , new
  , allowsNetworkAccess
  , setAllowsNetworkAccess
  , required
  , setRequired
  , configurationInfo
  , setConfigurationInfo
  , initSelector
  , newSelector
  , allowsNetworkAccessSelector
  , setAllowsNetworkAccessSelector
  , requiredSelector
  , setRequiredSelector
  , configurationInfoSelector
  , setConfigurationInfoSelector


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

-- | @- init@
init_ :: IsAEAssessmentParticipantConfiguration aeAssessmentParticipantConfiguration => aeAssessmentParticipantConfiguration -> IO (Id AEAssessmentParticipantConfiguration)
init_ aeAssessmentParticipantConfiguration  =
  sendMsg aeAssessmentParticipantConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AEAssessmentParticipantConfiguration)
new  =
  do
    cls' <- getRequiredClass "AEAssessmentParticipantConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- allowsNetworkAccess@
allowsNetworkAccess :: IsAEAssessmentParticipantConfiguration aeAssessmentParticipantConfiguration => aeAssessmentParticipantConfiguration -> IO Bool
allowsNetworkAccess aeAssessmentParticipantConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentParticipantConfiguration (mkSelector "allowsNetworkAccess") retCULong []

-- | @- setAllowsNetworkAccess:@
setAllowsNetworkAccess :: IsAEAssessmentParticipantConfiguration aeAssessmentParticipantConfiguration => aeAssessmentParticipantConfiguration -> Bool -> IO ()
setAllowsNetworkAccess aeAssessmentParticipantConfiguration  value =
  sendMsg aeAssessmentParticipantConfiguration (mkSelector "setAllowsNetworkAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | @- required@
required :: IsAEAssessmentParticipantConfiguration aeAssessmentParticipantConfiguration => aeAssessmentParticipantConfiguration -> IO Bool
required aeAssessmentParticipantConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentParticipantConfiguration (mkSelector "required") retCULong []

-- | @- setRequired:@
setRequired :: IsAEAssessmentParticipantConfiguration aeAssessmentParticipantConfiguration => aeAssessmentParticipantConfiguration -> Bool -> IO ()
setRequired aeAssessmentParticipantConfiguration  value =
  sendMsg aeAssessmentParticipantConfiguration (mkSelector "setRequired:") retVoid [argCULong (if value then 1 else 0)]

-- | @- configurationInfo@
configurationInfo :: IsAEAssessmentParticipantConfiguration aeAssessmentParticipantConfiguration => aeAssessmentParticipantConfiguration -> IO (Id NSDictionary)
configurationInfo aeAssessmentParticipantConfiguration  =
  sendMsg aeAssessmentParticipantConfiguration (mkSelector "configurationInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConfigurationInfo:@
setConfigurationInfo :: (IsAEAssessmentParticipantConfiguration aeAssessmentParticipantConfiguration, IsNSDictionary value) => aeAssessmentParticipantConfiguration -> value -> IO ()
setConfigurationInfo aeAssessmentParticipantConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg aeAssessmentParticipantConfiguration (mkSelector "setConfigurationInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @allowsNetworkAccess@
allowsNetworkAccessSelector :: Selector
allowsNetworkAccessSelector = mkSelector "allowsNetworkAccess"

-- | @Selector@ for @setAllowsNetworkAccess:@
setAllowsNetworkAccessSelector :: Selector
setAllowsNetworkAccessSelector = mkSelector "setAllowsNetworkAccess:"

-- | @Selector@ for @required@
requiredSelector :: Selector
requiredSelector = mkSelector "required"

-- | @Selector@ for @setRequired:@
setRequiredSelector :: Selector
setRequiredSelector = mkSelector "setRequired:"

-- | @Selector@ for @configurationInfo@
configurationInfoSelector :: Selector
configurationInfoSelector = mkSelector "configurationInfo"

-- | @Selector@ for @setConfigurationInfo:@
setConfigurationInfoSelector :: Selector
setConfigurationInfoSelector = mkSelector "setConfigurationInfo:"


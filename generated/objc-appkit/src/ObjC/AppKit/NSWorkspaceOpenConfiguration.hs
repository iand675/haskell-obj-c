{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWorkspaceOpenConfiguration@.
module ObjC.AppKit.NSWorkspaceOpenConfiguration
  ( NSWorkspaceOpenConfiguration
  , IsNSWorkspaceOpenConfiguration(..)
  , configuration
  , promptsUserIfNeeded
  , setPromptsUserIfNeeded
  , addsToRecentItems
  , setAddsToRecentItems
  , activates
  , setActivates
  , hides
  , setHides
  , hidesOthers
  , setHidesOthers
  , forPrinting
  , setForPrinting
  , createsNewApplicationInstance
  , setCreatesNewApplicationInstance
  , allowsRunningApplicationSubstitution
  , setAllowsRunningApplicationSubstitution
  , arguments
  , setArguments
  , environment
  , setEnvironment
  , appleEvent
  , setAppleEvent
  , architecture
  , setArchitecture
  , requiresUniversalLinks
  , setRequiresUniversalLinks
  , configurationSelector
  , promptsUserIfNeededSelector
  , setPromptsUserIfNeededSelector
  , addsToRecentItemsSelector
  , setAddsToRecentItemsSelector
  , activatesSelector
  , setActivatesSelector
  , hidesSelector
  , setHidesSelector
  , hidesOthersSelector
  , setHidesOthersSelector
  , forPrintingSelector
  , setForPrintingSelector
  , createsNewApplicationInstanceSelector
  , setCreatesNewApplicationInstanceSelector
  , allowsRunningApplicationSubstitutionSelector
  , setAllowsRunningApplicationSubstitutionSelector
  , argumentsSelector
  , setArgumentsSelector
  , environmentSelector
  , setEnvironmentSelector
  , appleEventSelector
  , setAppleEventSelector
  , architectureSelector
  , setArchitectureSelector
  , requiresUniversalLinksSelector
  , setRequiresUniversalLinksSelector


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

-- | @+ configuration@
configuration :: IO (Id NSWorkspaceOpenConfiguration)
configuration  =
  do
    cls' <- getRequiredClass "NSWorkspaceOpenConfiguration"
    sendClassMsg cls' (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- promptsUserIfNeeded@
promptsUserIfNeeded :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
promptsUserIfNeeded nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "promptsUserIfNeeded") retCULong []

-- | @- setPromptsUserIfNeeded:@
setPromptsUserIfNeeded :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setPromptsUserIfNeeded nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setPromptsUserIfNeeded:") retVoid [argCULong (if value then 1 else 0)]

-- | @- addsToRecentItems@
addsToRecentItems :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
addsToRecentItems nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "addsToRecentItems") retCULong []

-- | @- setAddsToRecentItems:@
setAddsToRecentItems :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setAddsToRecentItems nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setAddsToRecentItems:") retVoid [argCULong (if value then 1 else 0)]

-- | @- activates@
activates :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
activates nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "activates") retCULong []

-- | @- setActivates:@
setActivates :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setActivates nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setActivates:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hides@
hides :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
hides nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "hides") retCULong []

-- | @- setHides:@
setHides :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setHides nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setHides:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hidesOthers@
hidesOthers :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
hidesOthers nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "hidesOthers") retCULong []

-- | @- setHidesOthers:@
setHidesOthers :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setHidesOthers nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setHidesOthers:") retVoid [argCULong (if value then 1 else 0)]

-- | @- forPrinting@
forPrinting :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
forPrinting nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "forPrinting") retCULong []

-- | @- setForPrinting:@
setForPrinting :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setForPrinting nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setForPrinting:") retVoid [argCULong (if value then 1 else 0)]

-- | @- createsNewApplicationInstance@
createsNewApplicationInstance :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
createsNewApplicationInstance nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "createsNewApplicationInstance") retCULong []

-- | @- setCreatesNewApplicationInstance:@
setCreatesNewApplicationInstance :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setCreatesNewApplicationInstance nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setCreatesNewApplicationInstance:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsRunningApplicationSubstitution@
allowsRunningApplicationSubstitution :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
allowsRunningApplicationSubstitution nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "allowsRunningApplicationSubstitution") retCULong []

-- | @- setAllowsRunningApplicationSubstitution:@
setAllowsRunningApplicationSubstitution :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setAllowsRunningApplicationSubstitution nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setAllowsRunningApplicationSubstitution:") retVoid [argCULong (if value then 1 else 0)]

-- | @- arguments@
arguments :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO (Id NSArray)
arguments nsWorkspaceOpenConfiguration  =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "arguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArguments:@
setArguments :: (IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration, IsNSArray value) => nsWorkspaceOpenConfiguration -> value -> IO ()
setArguments nsWorkspaceOpenConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWorkspaceOpenConfiguration (mkSelector "setArguments:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- environment@
environment :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO (Id NSDictionary)
environment nsWorkspaceOpenConfiguration  =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "environment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnvironment:@
setEnvironment :: (IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration, IsNSDictionary value) => nsWorkspaceOpenConfiguration -> value -> IO ()
setEnvironment nsWorkspaceOpenConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWorkspaceOpenConfiguration (mkSelector "setEnvironment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- appleEvent@
appleEvent :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO (Id NSAppleEventDescriptor)
appleEvent nsWorkspaceOpenConfiguration  =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "appleEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAppleEvent:@
setAppleEvent :: (IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration, IsNSAppleEventDescriptor value) => nsWorkspaceOpenConfiguration -> value -> IO ()
setAppleEvent nsWorkspaceOpenConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsWorkspaceOpenConfiguration (mkSelector "setAppleEvent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- architecture@
architecture :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO CInt
architecture nsWorkspaceOpenConfiguration  =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "architecture") retCInt []

-- | @- setArchitecture:@
setArchitecture :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> CInt -> IO ()
setArchitecture nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setArchitecture:") retVoid [argCInt (fromIntegral value)]

-- | @- requiresUniversalLinks@
requiresUniversalLinks :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
requiresUniversalLinks nsWorkspaceOpenConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWorkspaceOpenConfiguration (mkSelector "requiresUniversalLinks") retCULong []

-- | @- setRequiresUniversalLinks:@
setRequiresUniversalLinks :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setRequiresUniversalLinks nsWorkspaceOpenConfiguration  value =
  sendMsg nsWorkspaceOpenConfiguration (mkSelector "setRequiresUniversalLinks:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @promptsUserIfNeeded@
promptsUserIfNeededSelector :: Selector
promptsUserIfNeededSelector = mkSelector "promptsUserIfNeeded"

-- | @Selector@ for @setPromptsUserIfNeeded:@
setPromptsUserIfNeededSelector :: Selector
setPromptsUserIfNeededSelector = mkSelector "setPromptsUserIfNeeded:"

-- | @Selector@ for @addsToRecentItems@
addsToRecentItemsSelector :: Selector
addsToRecentItemsSelector = mkSelector "addsToRecentItems"

-- | @Selector@ for @setAddsToRecentItems:@
setAddsToRecentItemsSelector :: Selector
setAddsToRecentItemsSelector = mkSelector "setAddsToRecentItems:"

-- | @Selector@ for @activates@
activatesSelector :: Selector
activatesSelector = mkSelector "activates"

-- | @Selector@ for @setActivates:@
setActivatesSelector :: Selector
setActivatesSelector = mkSelector "setActivates:"

-- | @Selector@ for @hides@
hidesSelector :: Selector
hidesSelector = mkSelector "hides"

-- | @Selector@ for @setHides:@
setHidesSelector :: Selector
setHidesSelector = mkSelector "setHides:"

-- | @Selector@ for @hidesOthers@
hidesOthersSelector :: Selector
hidesOthersSelector = mkSelector "hidesOthers"

-- | @Selector@ for @setHidesOthers:@
setHidesOthersSelector :: Selector
setHidesOthersSelector = mkSelector "setHidesOthers:"

-- | @Selector@ for @forPrinting@
forPrintingSelector :: Selector
forPrintingSelector = mkSelector "forPrinting"

-- | @Selector@ for @setForPrinting:@
setForPrintingSelector :: Selector
setForPrintingSelector = mkSelector "setForPrinting:"

-- | @Selector@ for @createsNewApplicationInstance@
createsNewApplicationInstanceSelector :: Selector
createsNewApplicationInstanceSelector = mkSelector "createsNewApplicationInstance"

-- | @Selector@ for @setCreatesNewApplicationInstance:@
setCreatesNewApplicationInstanceSelector :: Selector
setCreatesNewApplicationInstanceSelector = mkSelector "setCreatesNewApplicationInstance:"

-- | @Selector@ for @allowsRunningApplicationSubstitution@
allowsRunningApplicationSubstitutionSelector :: Selector
allowsRunningApplicationSubstitutionSelector = mkSelector "allowsRunningApplicationSubstitution"

-- | @Selector@ for @setAllowsRunningApplicationSubstitution:@
setAllowsRunningApplicationSubstitutionSelector :: Selector
setAllowsRunningApplicationSubstitutionSelector = mkSelector "setAllowsRunningApplicationSubstitution:"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @setArguments:@
setArgumentsSelector :: Selector
setArgumentsSelector = mkSelector "setArguments:"

-- | @Selector@ for @environment@
environmentSelector :: Selector
environmentSelector = mkSelector "environment"

-- | @Selector@ for @setEnvironment:@
setEnvironmentSelector :: Selector
setEnvironmentSelector = mkSelector "setEnvironment:"

-- | @Selector@ for @appleEvent@
appleEventSelector :: Selector
appleEventSelector = mkSelector "appleEvent"

-- | @Selector@ for @setAppleEvent:@
setAppleEventSelector :: Selector
setAppleEventSelector = mkSelector "setAppleEvent:"

-- | @Selector@ for @architecture@
architectureSelector :: Selector
architectureSelector = mkSelector "architecture"

-- | @Selector@ for @setArchitecture:@
setArchitectureSelector :: Selector
setArchitectureSelector = mkSelector "setArchitecture:"

-- | @Selector@ for @requiresUniversalLinks@
requiresUniversalLinksSelector :: Selector
requiresUniversalLinksSelector = mkSelector "requiresUniversalLinks"

-- | @Selector@ for @setRequiresUniversalLinks:@
setRequiresUniversalLinksSelector :: Selector
setRequiresUniversalLinksSelector = mkSelector "setRequiresUniversalLinks:"


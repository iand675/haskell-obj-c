{-# LANGUAGE DataKinds #-}
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
  , activatesSelector
  , addsToRecentItemsSelector
  , allowsRunningApplicationSubstitutionSelector
  , appleEventSelector
  , architectureSelector
  , argumentsSelector
  , configurationSelector
  , createsNewApplicationInstanceSelector
  , environmentSelector
  , forPrintingSelector
  , hidesOthersSelector
  , hidesSelector
  , promptsUserIfNeededSelector
  , requiresUniversalLinksSelector
  , setActivatesSelector
  , setAddsToRecentItemsSelector
  , setAllowsRunningApplicationSubstitutionSelector
  , setAppleEventSelector
  , setArchitectureSelector
  , setArgumentsSelector
  , setCreatesNewApplicationInstanceSelector
  , setEnvironmentSelector
  , setForPrintingSelector
  , setHidesOthersSelector
  , setHidesSelector
  , setPromptsUserIfNeededSelector
  , setRequiresUniversalLinksSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ configuration@
configuration :: IO (Id NSWorkspaceOpenConfiguration)
configuration  =
  do
    cls' <- getRequiredClass "NSWorkspaceOpenConfiguration"
    sendClassMessage cls' configurationSelector

-- | @- promptsUserIfNeeded@
promptsUserIfNeeded :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
promptsUserIfNeeded nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration promptsUserIfNeededSelector

-- | @- setPromptsUserIfNeeded:@
setPromptsUserIfNeeded :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setPromptsUserIfNeeded nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setPromptsUserIfNeededSelector value

-- | @- addsToRecentItems@
addsToRecentItems :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
addsToRecentItems nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration addsToRecentItemsSelector

-- | @- setAddsToRecentItems:@
setAddsToRecentItems :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setAddsToRecentItems nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setAddsToRecentItemsSelector value

-- | @- activates@
activates :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
activates nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration activatesSelector

-- | @- setActivates:@
setActivates :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setActivates nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setActivatesSelector value

-- | @- hides@
hides :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
hides nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration hidesSelector

-- | @- setHides:@
setHides :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setHides nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setHidesSelector value

-- | @- hidesOthers@
hidesOthers :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
hidesOthers nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration hidesOthersSelector

-- | @- setHidesOthers:@
setHidesOthers :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setHidesOthers nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setHidesOthersSelector value

-- | @- forPrinting@
forPrinting :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
forPrinting nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration forPrintingSelector

-- | @- setForPrinting:@
setForPrinting :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setForPrinting nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setForPrintingSelector value

-- | @- createsNewApplicationInstance@
createsNewApplicationInstance :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
createsNewApplicationInstance nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration createsNewApplicationInstanceSelector

-- | @- setCreatesNewApplicationInstance:@
setCreatesNewApplicationInstance :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setCreatesNewApplicationInstance nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setCreatesNewApplicationInstanceSelector value

-- | @- allowsRunningApplicationSubstitution@
allowsRunningApplicationSubstitution :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
allowsRunningApplicationSubstitution nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration allowsRunningApplicationSubstitutionSelector

-- | @- setAllowsRunningApplicationSubstitution:@
setAllowsRunningApplicationSubstitution :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setAllowsRunningApplicationSubstitution nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setAllowsRunningApplicationSubstitutionSelector value

-- | @- arguments@
arguments :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO (Id NSArray)
arguments nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration argumentsSelector

-- | @- setArguments:@
setArguments :: (IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration, IsNSArray value) => nsWorkspaceOpenConfiguration -> value -> IO ()
setArguments nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setArgumentsSelector (toNSArray value)

-- | @- environment@
environment :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO (Id NSDictionary)
environment nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration environmentSelector

-- | @- setEnvironment:@
setEnvironment :: (IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration, IsNSDictionary value) => nsWorkspaceOpenConfiguration -> value -> IO ()
setEnvironment nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setEnvironmentSelector (toNSDictionary value)

-- | @- appleEvent@
appleEvent :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO (Id NSAppleEventDescriptor)
appleEvent nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration appleEventSelector

-- | @- setAppleEvent:@
setAppleEvent :: (IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration, IsNSAppleEventDescriptor value) => nsWorkspaceOpenConfiguration -> value -> IO ()
setAppleEvent nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setAppleEventSelector (toNSAppleEventDescriptor value)

-- | @- architecture@
architecture :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO CInt
architecture nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration architectureSelector

-- | @- setArchitecture:@
setArchitecture :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> CInt -> IO ()
setArchitecture nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setArchitectureSelector value

-- | @- requiresUniversalLinks@
requiresUniversalLinks :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> IO Bool
requiresUniversalLinks nsWorkspaceOpenConfiguration =
  sendMessage nsWorkspaceOpenConfiguration requiresUniversalLinksSelector

-- | @- setRequiresUniversalLinks:@
setRequiresUniversalLinks :: IsNSWorkspaceOpenConfiguration nsWorkspaceOpenConfiguration => nsWorkspaceOpenConfiguration -> Bool -> IO ()
setRequiresUniversalLinks nsWorkspaceOpenConfiguration value =
  sendMessage nsWorkspaceOpenConfiguration setRequiresUniversalLinksSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id NSWorkspaceOpenConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @promptsUserIfNeeded@
promptsUserIfNeededSelector :: Selector '[] Bool
promptsUserIfNeededSelector = mkSelector "promptsUserIfNeeded"

-- | @Selector@ for @setPromptsUserIfNeeded:@
setPromptsUserIfNeededSelector :: Selector '[Bool] ()
setPromptsUserIfNeededSelector = mkSelector "setPromptsUserIfNeeded:"

-- | @Selector@ for @addsToRecentItems@
addsToRecentItemsSelector :: Selector '[] Bool
addsToRecentItemsSelector = mkSelector "addsToRecentItems"

-- | @Selector@ for @setAddsToRecentItems:@
setAddsToRecentItemsSelector :: Selector '[Bool] ()
setAddsToRecentItemsSelector = mkSelector "setAddsToRecentItems:"

-- | @Selector@ for @activates@
activatesSelector :: Selector '[] Bool
activatesSelector = mkSelector "activates"

-- | @Selector@ for @setActivates:@
setActivatesSelector :: Selector '[Bool] ()
setActivatesSelector = mkSelector "setActivates:"

-- | @Selector@ for @hides@
hidesSelector :: Selector '[] Bool
hidesSelector = mkSelector "hides"

-- | @Selector@ for @setHides:@
setHidesSelector :: Selector '[Bool] ()
setHidesSelector = mkSelector "setHides:"

-- | @Selector@ for @hidesOthers@
hidesOthersSelector :: Selector '[] Bool
hidesOthersSelector = mkSelector "hidesOthers"

-- | @Selector@ for @setHidesOthers:@
setHidesOthersSelector :: Selector '[Bool] ()
setHidesOthersSelector = mkSelector "setHidesOthers:"

-- | @Selector@ for @forPrinting@
forPrintingSelector :: Selector '[] Bool
forPrintingSelector = mkSelector "forPrinting"

-- | @Selector@ for @setForPrinting:@
setForPrintingSelector :: Selector '[Bool] ()
setForPrintingSelector = mkSelector "setForPrinting:"

-- | @Selector@ for @createsNewApplicationInstance@
createsNewApplicationInstanceSelector :: Selector '[] Bool
createsNewApplicationInstanceSelector = mkSelector "createsNewApplicationInstance"

-- | @Selector@ for @setCreatesNewApplicationInstance:@
setCreatesNewApplicationInstanceSelector :: Selector '[Bool] ()
setCreatesNewApplicationInstanceSelector = mkSelector "setCreatesNewApplicationInstance:"

-- | @Selector@ for @allowsRunningApplicationSubstitution@
allowsRunningApplicationSubstitutionSelector :: Selector '[] Bool
allowsRunningApplicationSubstitutionSelector = mkSelector "allowsRunningApplicationSubstitution"

-- | @Selector@ for @setAllowsRunningApplicationSubstitution:@
setAllowsRunningApplicationSubstitutionSelector :: Selector '[Bool] ()
setAllowsRunningApplicationSubstitutionSelector = mkSelector "setAllowsRunningApplicationSubstitution:"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSArray)
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @setArguments:@
setArgumentsSelector :: Selector '[Id NSArray] ()
setArgumentsSelector = mkSelector "setArguments:"

-- | @Selector@ for @environment@
environmentSelector :: Selector '[] (Id NSDictionary)
environmentSelector = mkSelector "environment"

-- | @Selector@ for @setEnvironment:@
setEnvironmentSelector :: Selector '[Id NSDictionary] ()
setEnvironmentSelector = mkSelector "setEnvironment:"

-- | @Selector@ for @appleEvent@
appleEventSelector :: Selector '[] (Id NSAppleEventDescriptor)
appleEventSelector = mkSelector "appleEvent"

-- | @Selector@ for @setAppleEvent:@
setAppleEventSelector :: Selector '[Id NSAppleEventDescriptor] ()
setAppleEventSelector = mkSelector "setAppleEvent:"

-- | @Selector@ for @architecture@
architectureSelector :: Selector '[] CInt
architectureSelector = mkSelector "architecture"

-- | @Selector@ for @setArchitecture:@
setArchitectureSelector :: Selector '[CInt] ()
setArchitectureSelector = mkSelector "setArchitecture:"

-- | @Selector@ for @requiresUniversalLinks@
requiresUniversalLinksSelector :: Selector '[] Bool
requiresUniversalLinksSelector = mkSelector "requiresUniversalLinks"

-- | @Selector@ for @setRequiresUniversalLinks:@
setRequiresUniversalLinksSelector :: Selector '[Bool] ()
setRequiresUniversalLinksSelector = mkSelector "setRequiresUniversalLinks:"


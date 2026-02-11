{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationCategory@.
module ObjC.UserNotifications.UNNotificationCategory
  ( UNNotificationCategory
  , IsUNNotificationCategory(..)
  , categoryWithIdentifier_actions_intentIdentifiers_options
  , categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_options
  , categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_options
  , init_
  , identifier
  , actions
  , intentIdentifiers
  , options
  , categoryWithIdentifier_actions_intentIdentifiers_optionsSelector
  , categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_optionsSelector
  , categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_optionsSelector
  , initSelector
  , identifierSelector
  , actionsSelector
  , intentIdentifiersSelector
  , optionsSelector

  -- * Enum types
  , UNNotificationCategoryOptions(UNNotificationCategoryOptions)
  , pattern UNNotificationCategoryOptionCustomDismissAction
  , pattern UNNotificationCategoryOptionAllowInCarPlay
  , pattern UNNotificationCategoryOptionHiddenPreviewsShowTitle
  , pattern UNNotificationCategoryOptionHiddenPreviewsShowSubtitle
  , pattern UNNotificationCategoryOptionAllowAnnouncement

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

import ObjC.UserNotifications.Internal.Classes
import ObjC.UserNotifications.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ categoryWithIdentifier:actions:intentIdentifiers:options:@
categoryWithIdentifier_actions_intentIdentifiers_options :: (IsNSString identifier, IsNSArray actions, IsNSArray intentIdentifiers) => identifier -> actions -> intentIdentifiers -> UNNotificationCategoryOptions -> IO (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_options identifier actions intentIdentifiers options =
  do
    cls' <- getRequiredClass "UNNotificationCategory"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr actions $ \raw_actions ->
        withObjCPtr intentIdentifiers $ \raw_intentIdentifiers ->
          sendClassMsg cls' (mkSelector "categoryWithIdentifier:actions:intentIdentifiers:options:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_intentIdentifiers :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_options :: (IsNSString identifier, IsNSArray actions, IsNSArray intentIdentifiers, IsNSString hiddenPreviewsBodyPlaceholder) => identifier -> actions -> intentIdentifiers -> hiddenPreviewsBodyPlaceholder -> UNNotificationCategoryOptions -> IO (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_options identifier actions intentIdentifiers hiddenPreviewsBodyPlaceholder options =
  do
    cls' <- getRequiredClass "UNNotificationCategory"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr actions $ \raw_actions ->
        withObjCPtr intentIdentifiers $ \raw_intentIdentifiers ->
          withObjCPtr hiddenPreviewsBodyPlaceholder $ \raw_hiddenPreviewsBodyPlaceholder ->
            sendClassMsg cls' (mkSelector "categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_intentIdentifiers :: Ptr ()), argPtr (castPtr raw_hiddenPreviewsBodyPlaceholder :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_options :: (IsNSString identifier, IsNSArray actions, IsNSArray intentIdentifiers, IsNSString hiddenPreviewsBodyPlaceholder, IsNSString categorySummaryFormat) => identifier -> actions -> intentIdentifiers -> hiddenPreviewsBodyPlaceholder -> categorySummaryFormat -> UNNotificationCategoryOptions -> IO (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_options identifier actions intentIdentifiers hiddenPreviewsBodyPlaceholder categorySummaryFormat options =
  do
    cls' <- getRequiredClass "UNNotificationCategory"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr actions $ \raw_actions ->
        withObjCPtr intentIdentifiers $ \raw_intentIdentifiers ->
          withObjCPtr hiddenPreviewsBodyPlaceholder $ \raw_hiddenPreviewsBodyPlaceholder ->
            withObjCPtr categorySummaryFormat $ \raw_categorySummaryFormat ->
              sendClassMsg cls' (mkSelector "categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_actions :: Ptr ()), argPtr (castPtr raw_intentIdentifiers :: Ptr ()), argPtr (castPtr raw_hiddenPreviewsBodyPlaceholder :: Ptr ()), argPtr (castPtr raw_categorySummaryFormat :: Ptr ()), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id UNNotificationCategory)
init_ unNotificationCategory  =
  sendMsg unNotificationCategory (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSString)
identifier unNotificationCategory  =
  sendMsg unNotificationCategory (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- actions@
actions :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSArray)
actions unNotificationCategory  =
  sendMsg unNotificationCategory (mkSelector "actions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- intentIdentifiers@
intentIdentifiers :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSArray)
intentIdentifiers unNotificationCategory  =
  sendMsg unNotificationCategory (mkSelector "intentIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- options@
options :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO UNNotificationCategoryOptions
options unNotificationCategory  =
  fmap (coerce :: CULong -> UNNotificationCategoryOptions) $ sendMsg unNotificationCategory (mkSelector "options") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @categoryWithIdentifier:actions:intentIdentifiers:options:@
categoryWithIdentifier_actions_intentIdentifiers_optionsSelector :: Selector
categoryWithIdentifier_actions_intentIdentifiers_optionsSelector = mkSelector "categoryWithIdentifier:actions:intentIdentifiers:options:"

-- | @Selector@ for @categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_optionsSelector :: Selector
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_optionsSelector = mkSelector "categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:"

-- | @Selector@ for @categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_optionsSelector :: Selector
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_optionsSelector = mkSelector "categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @actions@
actionsSelector :: Selector
actionsSelector = mkSelector "actions"

-- | @Selector@ for @intentIdentifiers@
intentIdentifiersSelector :: Selector
intentIdentifiersSelector = mkSelector "intentIdentifiers"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"


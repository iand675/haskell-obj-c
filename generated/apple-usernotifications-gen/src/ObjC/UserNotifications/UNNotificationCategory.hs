{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , hiddenPreviewsBodyPlaceholder
  , categorySummaryFormat
  , actionsSelector
  , categorySummaryFormatSelector
  , categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_optionsSelector
  , categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_optionsSelector
  , categoryWithIdentifier_actions_intentIdentifiers_optionsSelector
  , hiddenPreviewsBodyPlaceholderSelector
  , identifierSelector
  , initSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' categoryWithIdentifier_actions_intentIdentifiers_optionsSelector (toNSString identifier) (toNSArray actions) (toNSArray intentIdentifiers) options

-- | @+ categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_options :: (IsNSString identifier, IsNSArray actions, IsNSArray intentIdentifiers, IsNSString hiddenPreviewsBodyPlaceholder) => identifier -> actions -> intentIdentifiers -> hiddenPreviewsBodyPlaceholder -> UNNotificationCategoryOptions -> IO (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_options identifier actions intentIdentifiers hiddenPreviewsBodyPlaceholder options =
  do
    cls' <- getRequiredClass "UNNotificationCategory"
    sendClassMessage cls' categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_optionsSelector (toNSString identifier) (toNSArray actions) (toNSArray intentIdentifiers) (toNSString hiddenPreviewsBodyPlaceholder) options

-- | @+ categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_options :: (IsNSString identifier, IsNSArray actions, IsNSArray intentIdentifiers, IsNSString hiddenPreviewsBodyPlaceholder, IsNSString categorySummaryFormat) => identifier -> actions -> intentIdentifiers -> hiddenPreviewsBodyPlaceholder -> categorySummaryFormat -> UNNotificationCategoryOptions -> IO (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_options identifier actions intentIdentifiers hiddenPreviewsBodyPlaceholder categorySummaryFormat options =
  do
    cls' <- getRequiredClass "UNNotificationCategory"
    sendClassMessage cls' categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_optionsSelector (toNSString identifier) (toNSArray actions) (toNSArray intentIdentifiers) (toNSString hiddenPreviewsBodyPlaceholder) (toNSString categorySummaryFormat) options

-- | @- init@
init_ :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id UNNotificationCategory)
init_ unNotificationCategory =
  sendOwnedMessage unNotificationCategory initSelector

-- | @- identifier@
identifier :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSString)
identifier unNotificationCategory =
  sendMessage unNotificationCategory identifierSelector

-- | @- actions@
actions :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSArray)
actions unNotificationCategory =
  sendMessage unNotificationCategory actionsSelector

-- | @- intentIdentifiers@
intentIdentifiers :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSArray)
intentIdentifiers unNotificationCategory =
  sendMessage unNotificationCategory intentIdentifiersSelector

-- | @- options@
options :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO UNNotificationCategoryOptions
options unNotificationCategory =
  sendMessage unNotificationCategory optionsSelector

-- | @- hiddenPreviewsBodyPlaceholder@
hiddenPreviewsBodyPlaceholder :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSString)
hiddenPreviewsBodyPlaceholder unNotificationCategory =
  sendMessage unNotificationCategory hiddenPreviewsBodyPlaceholderSelector

-- | A format string for a summary description when notifications from this category are grouped together. It should contain descriptive text and format arguments that will be replaced with the information from the notifications that have been grouped together. The arguments are replaced with the number of notifications and the list created by joining the argument in each grouped notification. For example: "%u new messages from %\@". The arguments list is optional, "%u new messages" is also accepted.
--
-- ObjC selector: @- categorySummaryFormat@
categorySummaryFormat :: IsUNNotificationCategory unNotificationCategory => unNotificationCategory -> IO (Id NSString)
categorySummaryFormat unNotificationCategory =
  sendMessage unNotificationCategory categorySummaryFormatSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @categoryWithIdentifier:actions:intentIdentifiers:options:@
categoryWithIdentifier_actions_intentIdentifiers_optionsSelector :: Selector '[Id NSString, Id NSArray, Id NSArray, UNNotificationCategoryOptions] (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_optionsSelector = mkSelector "categoryWithIdentifier:actions:intentIdentifiers:options:"

-- | @Selector@ for @categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_optionsSelector :: Selector '[Id NSString, Id NSArray, Id NSArray, Id NSString, UNNotificationCategoryOptions] (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_optionsSelector = mkSelector "categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:"

-- | @Selector@ for @categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:@
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_optionsSelector :: Selector '[Id NSString, Id NSArray, Id NSArray, Id NSString, Id NSString, UNNotificationCategoryOptions] (Id UNNotificationCategory)
categoryWithIdentifier_actions_intentIdentifiers_hiddenPreviewsBodyPlaceholder_categorySummaryFormat_optionsSelector = mkSelector "categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationCategory)
initSelector = mkSelector "init"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @actions@
actionsSelector :: Selector '[] (Id NSArray)
actionsSelector = mkSelector "actions"

-- | @Selector@ for @intentIdentifiers@
intentIdentifiersSelector :: Selector '[] (Id NSArray)
intentIdentifiersSelector = mkSelector "intentIdentifiers"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] UNNotificationCategoryOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @hiddenPreviewsBodyPlaceholder@
hiddenPreviewsBodyPlaceholderSelector :: Selector '[] (Id NSString)
hiddenPreviewsBodyPlaceholderSelector = mkSelector "hiddenPreviewsBodyPlaceholder"

-- | @Selector@ for @categorySummaryFormat@
categorySummaryFormatSelector :: Selector '[] (Id NSString)
categorySummaryFormatSelector = mkSelector "categorySummaryFormat"


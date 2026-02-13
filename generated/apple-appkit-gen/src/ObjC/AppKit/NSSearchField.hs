{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSearchField@.
module ObjC.AppKit.NSSearchField
  ( NSSearchField
  , IsNSSearchField(..)
  , rectForSearchTextWhenCentered
  , rectForSearchButtonWhenCentered
  , rectForCancelButtonWhenCentered
  , searchTextBounds
  , searchButtonBounds
  , cancelButtonBounds
  , recentSearches
  , setRecentSearches
  , recentsAutosaveName
  , setRecentsAutosaveName
  , searchMenuTemplate
  , setSearchMenuTemplate
  , sendsWholeSearchString
  , setSendsWholeSearchString
  , maximumRecents
  , setMaximumRecents
  , sendsSearchStringImmediately
  , setSendsSearchStringImmediately
  , delegate
  , setDelegate
  , centersPlaceholder
  , setCentersPlaceholder
  , cancelButtonBoundsSelector
  , centersPlaceholderSelector
  , delegateSelector
  , maximumRecentsSelector
  , recentSearchesSelector
  , recentsAutosaveNameSelector
  , rectForCancelButtonWhenCenteredSelector
  , rectForSearchButtonWhenCenteredSelector
  , rectForSearchTextWhenCenteredSelector
  , searchButtonBoundsSelector
  , searchMenuTemplateSelector
  , searchTextBoundsSelector
  , sendsSearchStringImmediatelySelector
  , sendsWholeSearchStringSelector
  , setCentersPlaceholderSelector
  , setDelegateSelector
  , setMaximumRecentsSelector
  , setRecentSearchesSelector
  , setRecentsAutosaveNameSelector
  , setSearchMenuTemplateSelector
  , setSendsSearchStringImmediatelySelector
  , setSendsWholeSearchStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- rectForSearchTextWhenCentered:@
rectForSearchTextWhenCentered :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO NSRect
rectForSearchTextWhenCentered nsSearchField isCentered =
  sendMessage nsSearchField rectForSearchTextWhenCenteredSelector isCentered

-- | @- rectForSearchButtonWhenCentered:@
rectForSearchButtonWhenCentered :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO NSRect
rectForSearchButtonWhenCentered nsSearchField isCentered =
  sendMessage nsSearchField rectForSearchButtonWhenCenteredSelector isCentered

-- | @- rectForCancelButtonWhenCentered:@
rectForCancelButtonWhenCentered :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO NSRect
rectForCancelButtonWhenCentered nsSearchField isCentered =
  sendMessage nsSearchField rectForCancelButtonWhenCenteredSelector isCentered

-- | @- searchTextBounds@
searchTextBounds :: IsNSSearchField nsSearchField => nsSearchField -> IO NSRect
searchTextBounds nsSearchField =
  sendMessage nsSearchField searchTextBoundsSelector

-- | @- searchButtonBounds@
searchButtonBounds :: IsNSSearchField nsSearchField => nsSearchField -> IO NSRect
searchButtonBounds nsSearchField =
  sendMessage nsSearchField searchButtonBoundsSelector

-- | @- cancelButtonBounds@
cancelButtonBounds :: IsNSSearchField nsSearchField => nsSearchField -> IO NSRect
cancelButtonBounds nsSearchField =
  sendMessage nsSearchField cancelButtonBoundsSelector

-- | @- recentSearches@
recentSearches :: IsNSSearchField nsSearchField => nsSearchField -> IO (Id NSArray)
recentSearches nsSearchField =
  sendMessage nsSearchField recentSearchesSelector

-- | @- setRecentSearches:@
setRecentSearches :: (IsNSSearchField nsSearchField, IsNSArray value) => nsSearchField -> value -> IO ()
setRecentSearches nsSearchField value =
  sendMessage nsSearchField setRecentSearchesSelector (toNSArray value)

-- | @- recentsAutosaveName@
recentsAutosaveName :: IsNSSearchField nsSearchField => nsSearchField -> IO (Id NSString)
recentsAutosaveName nsSearchField =
  sendMessage nsSearchField recentsAutosaveNameSelector

-- | @- setRecentsAutosaveName:@
setRecentsAutosaveName :: (IsNSSearchField nsSearchField, IsNSString value) => nsSearchField -> value -> IO ()
setRecentsAutosaveName nsSearchField value =
  sendMessage nsSearchField setRecentsAutosaveNameSelector (toNSString value)

-- | @- searchMenuTemplate@
searchMenuTemplate :: IsNSSearchField nsSearchField => nsSearchField -> IO (Id NSMenu)
searchMenuTemplate nsSearchField =
  sendMessage nsSearchField searchMenuTemplateSelector

-- | @- setSearchMenuTemplate:@
setSearchMenuTemplate :: (IsNSSearchField nsSearchField, IsNSMenu value) => nsSearchField -> value -> IO ()
setSearchMenuTemplate nsSearchField value =
  sendMessage nsSearchField setSearchMenuTemplateSelector (toNSMenu value)

-- | @- sendsWholeSearchString@
sendsWholeSearchString :: IsNSSearchField nsSearchField => nsSearchField -> IO Bool
sendsWholeSearchString nsSearchField =
  sendMessage nsSearchField sendsWholeSearchStringSelector

-- | @- setSendsWholeSearchString:@
setSendsWholeSearchString :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO ()
setSendsWholeSearchString nsSearchField value =
  sendMessage nsSearchField setSendsWholeSearchStringSelector value

-- | @- maximumRecents@
maximumRecents :: IsNSSearchField nsSearchField => nsSearchField -> IO CLong
maximumRecents nsSearchField =
  sendMessage nsSearchField maximumRecentsSelector

-- | @- setMaximumRecents:@
setMaximumRecents :: IsNSSearchField nsSearchField => nsSearchField -> CLong -> IO ()
setMaximumRecents nsSearchField value =
  sendMessage nsSearchField setMaximumRecentsSelector value

-- | @- sendsSearchStringImmediately@
sendsSearchStringImmediately :: IsNSSearchField nsSearchField => nsSearchField -> IO Bool
sendsSearchStringImmediately nsSearchField =
  sendMessage nsSearchField sendsSearchStringImmediatelySelector

-- | @- setSendsSearchStringImmediately:@
setSendsSearchStringImmediately :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO ()
setSendsSearchStringImmediately nsSearchField value =
  sendMessage nsSearchField setSendsSearchStringImmediatelySelector value

-- | @- delegate@
delegate :: IsNSSearchField nsSearchField => nsSearchField -> IO RawId
delegate nsSearchField =
  sendMessage nsSearchField delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSearchField nsSearchField => nsSearchField -> RawId -> IO ()
setDelegate nsSearchField value =
  sendMessage nsSearchField setDelegateSelector value

-- | @- centersPlaceholder@
centersPlaceholder :: IsNSSearchField nsSearchField => nsSearchField -> IO Bool
centersPlaceholder nsSearchField =
  sendMessage nsSearchField centersPlaceholderSelector

-- | @- setCentersPlaceholder:@
setCentersPlaceholder :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO ()
setCentersPlaceholder nsSearchField value =
  sendMessage nsSearchField setCentersPlaceholderSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rectForSearchTextWhenCentered:@
rectForSearchTextWhenCenteredSelector :: Selector '[Bool] NSRect
rectForSearchTextWhenCenteredSelector = mkSelector "rectForSearchTextWhenCentered:"

-- | @Selector@ for @rectForSearchButtonWhenCentered:@
rectForSearchButtonWhenCenteredSelector :: Selector '[Bool] NSRect
rectForSearchButtonWhenCenteredSelector = mkSelector "rectForSearchButtonWhenCentered:"

-- | @Selector@ for @rectForCancelButtonWhenCentered:@
rectForCancelButtonWhenCenteredSelector :: Selector '[Bool] NSRect
rectForCancelButtonWhenCenteredSelector = mkSelector "rectForCancelButtonWhenCentered:"

-- | @Selector@ for @searchTextBounds@
searchTextBoundsSelector :: Selector '[] NSRect
searchTextBoundsSelector = mkSelector "searchTextBounds"

-- | @Selector@ for @searchButtonBounds@
searchButtonBoundsSelector :: Selector '[] NSRect
searchButtonBoundsSelector = mkSelector "searchButtonBounds"

-- | @Selector@ for @cancelButtonBounds@
cancelButtonBoundsSelector :: Selector '[] NSRect
cancelButtonBoundsSelector = mkSelector "cancelButtonBounds"

-- | @Selector@ for @recentSearches@
recentSearchesSelector :: Selector '[] (Id NSArray)
recentSearchesSelector = mkSelector "recentSearches"

-- | @Selector@ for @setRecentSearches:@
setRecentSearchesSelector :: Selector '[Id NSArray] ()
setRecentSearchesSelector = mkSelector "setRecentSearches:"

-- | @Selector@ for @recentsAutosaveName@
recentsAutosaveNameSelector :: Selector '[] (Id NSString)
recentsAutosaveNameSelector = mkSelector "recentsAutosaveName"

-- | @Selector@ for @setRecentsAutosaveName:@
setRecentsAutosaveNameSelector :: Selector '[Id NSString] ()
setRecentsAutosaveNameSelector = mkSelector "setRecentsAutosaveName:"

-- | @Selector@ for @searchMenuTemplate@
searchMenuTemplateSelector :: Selector '[] (Id NSMenu)
searchMenuTemplateSelector = mkSelector "searchMenuTemplate"

-- | @Selector@ for @setSearchMenuTemplate:@
setSearchMenuTemplateSelector :: Selector '[Id NSMenu] ()
setSearchMenuTemplateSelector = mkSelector "setSearchMenuTemplate:"

-- | @Selector@ for @sendsWholeSearchString@
sendsWholeSearchStringSelector :: Selector '[] Bool
sendsWholeSearchStringSelector = mkSelector "sendsWholeSearchString"

-- | @Selector@ for @setSendsWholeSearchString:@
setSendsWholeSearchStringSelector :: Selector '[Bool] ()
setSendsWholeSearchStringSelector = mkSelector "setSendsWholeSearchString:"

-- | @Selector@ for @maximumRecents@
maximumRecentsSelector :: Selector '[] CLong
maximumRecentsSelector = mkSelector "maximumRecents"

-- | @Selector@ for @setMaximumRecents:@
setMaximumRecentsSelector :: Selector '[CLong] ()
setMaximumRecentsSelector = mkSelector "setMaximumRecents:"

-- | @Selector@ for @sendsSearchStringImmediately@
sendsSearchStringImmediatelySelector :: Selector '[] Bool
sendsSearchStringImmediatelySelector = mkSelector "sendsSearchStringImmediately"

-- | @Selector@ for @setSendsSearchStringImmediately:@
setSendsSearchStringImmediatelySelector :: Selector '[Bool] ()
setSendsSearchStringImmediatelySelector = mkSelector "setSendsSearchStringImmediately:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @centersPlaceholder@
centersPlaceholderSelector :: Selector '[] Bool
centersPlaceholderSelector = mkSelector "centersPlaceholder"

-- | @Selector@ for @setCentersPlaceholder:@
setCentersPlaceholderSelector :: Selector '[Bool] ()
setCentersPlaceholderSelector = mkSelector "setCentersPlaceholder:"


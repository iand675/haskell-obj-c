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
  , rectForSearchTextWhenCenteredSelector
  , rectForSearchButtonWhenCenteredSelector
  , rectForCancelButtonWhenCenteredSelector
  , searchTextBoundsSelector
  , searchButtonBoundsSelector
  , cancelButtonBoundsSelector
  , recentSearchesSelector
  , setRecentSearchesSelector
  , recentsAutosaveNameSelector
  , setRecentsAutosaveNameSelector
  , searchMenuTemplateSelector
  , setSearchMenuTemplateSelector
  , sendsWholeSearchStringSelector
  , setSendsWholeSearchStringSelector
  , maximumRecentsSelector
  , setMaximumRecentsSelector
  , sendsSearchStringImmediatelySelector
  , setSendsSearchStringImmediatelySelector
  , delegateSelector
  , setDelegateSelector
  , centersPlaceholderSelector
  , setCentersPlaceholderSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- rectForSearchTextWhenCentered:@
rectForSearchTextWhenCentered :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO NSRect
rectForSearchTextWhenCentered nsSearchField  isCentered =
    sendMsgStret nsSearchField (mkSelector "rectForSearchTextWhenCentered:") retNSRect [argCULong (if isCentered then 1 else 0)]

-- | @- rectForSearchButtonWhenCentered:@
rectForSearchButtonWhenCentered :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO NSRect
rectForSearchButtonWhenCentered nsSearchField  isCentered =
    sendMsgStret nsSearchField (mkSelector "rectForSearchButtonWhenCentered:") retNSRect [argCULong (if isCentered then 1 else 0)]

-- | @- rectForCancelButtonWhenCentered:@
rectForCancelButtonWhenCentered :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO NSRect
rectForCancelButtonWhenCentered nsSearchField  isCentered =
    sendMsgStret nsSearchField (mkSelector "rectForCancelButtonWhenCentered:") retNSRect [argCULong (if isCentered then 1 else 0)]

-- | @- searchTextBounds@
searchTextBounds :: IsNSSearchField nsSearchField => nsSearchField -> IO NSRect
searchTextBounds nsSearchField  =
    sendMsgStret nsSearchField (mkSelector "searchTextBounds") retNSRect []

-- | @- searchButtonBounds@
searchButtonBounds :: IsNSSearchField nsSearchField => nsSearchField -> IO NSRect
searchButtonBounds nsSearchField  =
    sendMsgStret nsSearchField (mkSelector "searchButtonBounds") retNSRect []

-- | @- cancelButtonBounds@
cancelButtonBounds :: IsNSSearchField nsSearchField => nsSearchField -> IO NSRect
cancelButtonBounds nsSearchField  =
    sendMsgStret nsSearchField (mkSelector "cancelButtonBounds") retNSRect []

-- | @- recentSearches@
recentSearches :: IsNSSearchField nsSearchField => nsSearchField -> IO (Id NSArray)
recentSearches nsSearchField  =
    sendMsg nsSearchField (mkSelector "recentSearches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecentSearches:@
setRecentSearches :: (IsNSSearchField nsSearchField, IsNSArray value) => nsSearchField -> value -> IO ()
setRecentSearches nsSearchField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSearchField (mkSelector "setRecentSearches:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- recentsAutosaveName@
recentsAutosaveName :: IsNSSearchField nsSearchField => nsSearchField -> IO (Id NSString)
recentsAutosaveName nsSearchField  =
    sendMsg nsSearchField (mkSelector "recentsAutosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecentsAutosaveName:@
setRecentsAutosaveName :: (IsNSSearchField nsSearchField, IsNSString value) => nsSearchField -> value -> IO ()
setRecentsAutosaveName nsSearchField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSearchField (mkSelector "setRecentsAutosaveName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- searchMenuTemplate@
searchMenuTemplate :: IsNSSearchField nsSearchField => nsSearchField -> IO (Id NSMenu)
searchMenuTemplate nsSearchField  =
    sendMsg nsSearchField (mkSelector "searchMenuTemplate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchMenuTemplate:@
setSearchMenuTemplate :: (IsNSSearchField nsSearchField, IsNSMenu value) => nsSearchField -> value -> IO ()
setSearchMenuTemplate nsSearchField  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSearchField (mkSelector "setSearchMenuTemplate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sendsWholeSearchString@
sendsWholeSearchString :: IsNSSearchField nsSearchField => nsSearchField -> IO Bool
sendsWholeSearchString nsSearchField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSearchField (mkSelector "sendsWholeSearchString") retCULong []

-- | @- setSendsWholeSearchString:@
setSendsWholeSearchString :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO ()
setSendsWholeSearchString nsSearchField  value =
    sendMsg nsSearchField (mkSelector "setSendsWholeSearchString:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maximumRecents@
maximumRecents :: IsNSSearchField nsSearchField => nsSearchField -> IO CLong
maximumRecents nsSearchField  =
    sendMsg nsSearchField (mkSelector "maximumRecents") retCLong []

-- | @- setMaximumRecents:@
setMaximumRecents :: IsNSSearchField nsSearchField => nsSearchField -> CLong -> IO ()
setMaximumRecents nsSearchField  value =
    sendMsg nsSearchField (mkSelector "setMaximumRecents:") retVoid [argCLong value]

-- | @- sendsSearchStringImmediately@
sendsSearchStringImmediately :: IsNSSearchField nsSearchField => nsSearchField -> IO Bool
sendsSearchStringImmediately nsSearchField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSearchField (mkSelector "sendsSearchStringImmediately") retCULong []

-- | @- setSendsSearchStringImmediately:@
setSendsSearchStringImmediately :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO ()
setSendsSearchStringImmediately nsSearchField  value =
    sendMsg nsSearchField (mkSelector "setSendsSearchStringImmediately:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delegate@
delegate :: IsNSSearchField nsSearchField => nsSearchField -> IO RawId
delegate nsSearchField  =
    fmap (RawId . castPtr) $ sendMsg nsSearchField (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSSearchField nsSearchField => nsSearchField -> RawId -> IO ()
setDelegate nsSearchField  value =
    sendMsg nsSearchField (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- centersPlaceholder@
centersPlaceholder :: IsNSSearchField nsSearchField => nsSearchField -> IO Bool
centersPlaceholder nsSearchField  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSearchField (mkSelector "centersPlaceholder") retCULong []

-- | @- setCentersPlaceholder:@
setCentersPlaceholder :: IsNSSearchField nsSearchField => nsSearchField -> Bool -> IO ()
setCentersPlaceholder nsSearchField  value =
    sendMsg nsSearchField (mkSelector "setCentersPlaceholder:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rectForSearchTextWhenCentered:@
rectForSearchTextWhenCenteredSelector :: Selector
rectForSearchTextWhenCenteredSelector = mkSelector "rectForSearchTextWhenCentered:"

-- | @Selector@ for @rectForSearchButtonWhenCentered:@
rectForSearchButtonWhenCenteredSelector :: Selector
rectForSearchButtonWhenCenteredSelector = mkSelector "rectForSearchButtonWhenCentered:"

-- | @Selector@ for @rectForCancelButtonWhenCentered:@
rectForCancelButtonWhenCenteredSelector :: Selector
rectForCancelButtonWhenCenteredSelector = mkSelector "rectForCancelButtonWhenCentered:"

-- | @Selector@ for @searchTextBounds@
searchTextBoundsSelector :: Selector
searchTextBoundsSelector = mkSelector "searchTextBounds"

-- | @Selector@ for @searchButtonBounds@
searchButtonBoundsSelector :: Selector
searchButtonBoundsSelector = mkSelector "searchButtonBounds"

-- | @Selector@ for @cancelButtonBounds@
cancelButtonBoundsSelector :: Selector
cancelButtonBoundsSelector = mkSelector "cancelButtonBounds"

-- | @Selector@ for @recentSearches@
recentSearchesSelector :: Selector
recentSearchesSelector = mkSelector "recentSearches"

-- | @Selector@ for @setRecentSearches:@
setRecentSearchesSelector :: Selector
setRecentSearchesSelector = mkSelector "setRecentSearches:"

-- | @Selector@ for @recentsAutosaveName@
recentsAutosaveNameSelector :: Selector
recentsAutosaveNameSelector = mkSelector "recentsAutosaveName"

-- | @Selector@ for @setRecentsAutosaveName:@
setRecentsAutosaveNameSelector :: Selector
setRecentsAutosaveNameSelector = mkSelector "setRecentsAutosaveName:"

-- | @Selector@ for @searchMenuTemplate@
searchMenuTemplateSelector :: Selector
searchMenuTemplateSelector = mkSelector "searchMenuTemplate"

-- | @Selector@ for @setSearchMenuTemplate:@
setSearchMenuTemplateSelector :: Selector
setSearchMenuTemplateSelector = mkSelector "setSearchMenuTemplate:"

-- | @Selector@ for @sendsWholeSearchString@
sendsWholeSearchStringSelector :: Selector
sendsWholeSearchStringSelector = mkSelector "sendsWholeSearchString"

-- | @Selector@ for @setSendsWholeSearchString:@
setSendsWholeSearchStringSelector :: Selector
setSendsWholeSearchStringSelector = mkSelector "setSendsWholeSearchString:"

-- | @Selector@ for @maximumRecents@
maximumRecentsSelector :: Selector
maximumRecentsSelector = mkSelector "maximumRecents"

-- | @Selector@ for @setMaximumRecents:@
setMaximumRecentsSelector :: Selector
setMaximumRecentsSelector = mkSelector "setMaximumRecents:"

-- | @Selector@ for @sendsSearchStringImmediately@
sendsSearchStringImmediatelySelector :: Selector
sendsSearchStringImmediatelySelector = mkSelector "sendsSearchStringImmediately"

-- | @Selector@ for @setSendsSearchStringImmediately:@
setSendsSearchStringImmediatelySelector :: Selector
setSendsSearchStringImmediatelySelector = mkSelector "setSendsSearchStringImmediately:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @centersPlaceholder@
centersPlaceholderSelector :: Selector
centersPlaceholderSelector = mkSelector "centersPlaceholder"

-- | @Selector@ for @setCentersPlaceholder:@
setCentersPlaceholderSelector :: Selector
setCentersPlaceholderSelector = mkSelector "setCentersPlaceholder:"


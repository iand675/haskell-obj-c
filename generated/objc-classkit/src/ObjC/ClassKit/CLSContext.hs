{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contexts represent activities, documents, and areas within your app.
--
-- Contexts have two major components.
--
-- (1) Child contexts, used to model your app hierarchy.                (2) Activity, holds user generated data that pertains to this context.
--
-- Generated bindings for @CLSContext@.
module ObjC.ClassKit.CLSContext
  ( CLSContext
  , IsCLSContext(..)
  , new
  , init_
  , initWithType_identifier_title
  , becomeActive
  , resignActive
  , setType
  , addProgressReportingCapabilities
  , resetProgressReportingCapabilities
  , createNewActivity
  , removeFromParent
  , addChildContext
  , descendantMatchingIdentifierPath_completion
  , addNavigationChildContext
  , removeNavigationChildContext
  , identifierPath
  , identifier
  , universalLinkURL
  , setUniversalLinkURL
  , type_
  , customTypeName
  , setCustomTypeName
  , title
  , setTitle
  , displayOrder
  , setDisplayOrder
  , topic
  , setTopic
  , assignable
  , setAssignable
  , suggestedAge
  , setSuggestedAge
  , suggestedCompletionTime
  , setSuggestedCompletionTime
  , progressReportingCapabilities
  , summary
  , setSummary
  , thumbnail
  , setThumbnail
  , active
  , currentActivity
  , parent
  , navigationChildContexts
  , newSelector
  , initSelector
  , initWithType_identifier_titleSelector
  , becomeActiveSelector
  , resignActiveSelector
  , setTypeSelector
  , addProgressReportingCapabilitiesSelector
  , resetProgressReportingCapabilitiesSelector
  , createNewActivitySelector
  , removeFromParentSelector
  , addChildContextSelector
  , descendantMatchingIdentifierPath_completionSelector
  , addNavigationChildContextSelector
  , removeNavigationChildContextSelector
  , identifierPathSelector
  , identifierSelector
  , universalLinkURLSelector
  , setUniversalLinkURLSelector
  , typeSelector
  , customTypeNameSelector
  , setCustomTypeNameSelector
  , titleSelector
  , setTitleSelector
  , displayOrderSelector
  , setDisplayOrderSelector
  , topicSelector
  , setTopicSelector
  , assignableSelector
  , setAssignableSelector
  , suggestedAgeSelector
  , setSuggestedAgeSelector
  , suggestedCompletionTimeSelector
  , setSuggestedCompletionTimeSelector
  , progressReportingCapabilitiesSelector
  , summarySelector
  , setSummarySelector
  , thumbnailSelector
  , setThumbnailSelector
  , activeSelector
  , currentActivitySelector
  , parentSelector
  , navigationChildContextsSelector

  -- * Enum types
  , CLSContextType(CLSContextType)
  , pattern CLSContextTypeNone
  , pattern CLSContextTypeApp
  , pattern CLSContextTypeChapter
  , pattern CLSContextTypeSection
  , pattern CLSContextTypeLevel
  , pattern CLSContextTypePage
  , pattern CLSContextTypeTask
  , pattern CLSContextTypeChallenge
  , pattern CLSContextTypeQuiz
  , pattern CLSContextTypeExercise
  , pattern CLSContextTypeLesson
  , pattern CLSContextTypeBook
  , pattern CLSContextTypeGame
  , pattern CLSContextTypeDocument
  , pattern CLSContextTypeAudio
  , pattern CLSContextTypeVideo
  , pattern CLSContextTypeCourse
  , pattern CLSContextTypeCustom

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

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.ClassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CLSContext)
new  =
  do
    cls' <- getRequiredClass "CLSContext"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCLSContext clsContext => clsContext -> IO (Id CLSContext)
init_ clsContext  =
  sendMsg clsContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize and configure the type of content this context represents.
--
-- @identifier@ — App-assigned identifier for this context. 256 characters max length.
--
-- @type@ — The type of content this context represents.
--
-- @title@ — Title for what this context represents. 256 characters max length.
--
-- ObjC selector: @- initWithType:identifier:title:@
initWithType_identifier_title :: (IsCLSContext clsContext, IsNSString identifier, IsNSString title) => clsContext -> CLSContextType -> identifier -> title -> IO (Id CLSContext)
initWithType_identifier_title clsContext  type_ identifier title =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr title $ \raw_title ->
      sendMsg clsContext (mkSelector "initWithType:identifier:title:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ())] >>= ownedObject . castPtr

-- | Marks contexts as active.
--
-- If a context is already active, it will remain active. If another context is active, the other will resign active before this one becomes active.
--
-- ObjC selector: @- becomeActive@
becomeActive :: IsCLSContext clsContext => clsContext -> IO ()
becomeActive clsContext  =
  sendMsg clsContext (mkSelector "becomeActive") retVoid []

-- | Resign being active.
--
-- This method does nothing if the reciever of the message is not active.
--
-- ObjC selector: @- resignActive@
resignActive :: IsCLSContext clsContext => clsContext -> IO ()
resignActive clsContext  =
  sendMsg clsContext (mkSelector "resignActive") retVoid []

-- | Sets the type.
--
-- Use this to update a previously saved context.
--
-- ObjC selector: @- setType:@
setType :: IsCLSContext clsContext => clsContext -> CLSContextType -> IO ()
setType clsContext  type_ =
  sendMsg clsContext (mkSelector "setType:") retVoid [argCLong (coerce type_)]

-- | Add or replace additional progress reporting capabilities of the app for this context.
--
-- If this parameter contains multiple items with the same value for kind, then one of them will be arbitrarily selected and used. If this parameter contains a capability of kind CLSProgressReportingCapabilityKindDuration, it will be ignored.
--
-- @capabilities@ — Progress reporting capabilities to add or replace existing capabilties.
--
-- ObjC selector: @- addProgressReportingCapabilities:@
addProgressReportingCapabilities :: (IsCLSContext clsContext, IsNSSet capabilities) => clsContext -> capabilities -> IO ()
addProgressReportingCapabilities clsContext  capabilities =
withObjCPtr capabilities $ \raw_capabilities ->
    sendMsg clsContext (mkSelector "addProgressReportingCapabilities:") retVoid [argPtr (castPtr raw_capabilities :: Ptr ())]

-- | Clears CLSProgressReportingCapability objects added to the receiver.
--
-- Removes all capabilities added via '-addProgressReportingCapabilities:'. The context will have the default progress reporting capability of kind CLSProgressReportingCapabilityKindDuration.
--
-- ObjC selector: @- resetProgressReportingCapabilities@
resetProgressReportingCapabilities :: IsCLSContext clsContext => clsContext -> IO ()
resetProgressReportingCapabilities clsContext  =
  sendMsg clsContext (mkSelector "resetProgressReportingCapabilities") retVoid []

-- | Creates a new activity
--
-- Creates a new activity and sets it as the current activity.
--
-- ObjC selector: @- createNewActivity@
createNewActivity :: IsCLSContext clsContext => clsContext -> IO (Id CLSActivity)
createNewActivity clsContext  =
  sendMsg clsContext (mkSelector "createNewActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Removes this child context from its parent.
--
-- If you remove a context from its parent and do not add it as a child of another context, it will be deleted when you call -save on the dataStore.
--
-- ObjC selector: @- removeFromParent@
removeFromParent :: IsCLSContext clsContext => clsContext -> IO ()
removeFromParent clsContext  =
  sendMsg clsContext (mkSelector "removeFromParent") retVoid []

-- | Adds a child context.
--
-- A context can only have a single parent.
--
-- Note: objectID of child context may change after it's been added.
--
-- ObjC selector: @- addChildContext:@
addChildContext :: (IsCLSContext clsContext, IsCLSContext child) => clsContext -> child -> IO ()
addChildContext clsContext  child =
withObjCPtr child $ \raw_child ->
    sendMsg clsContext (mkSelector "addChildContext:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | Returns a descendant of this context matching the context path you provide. Context path must start with an identifier of a child context of the context to which this message is sent.
--
-- If there are any missing contexts, they will be filled in by calling the following method on the context's data store's delegate:
--
-- -[CLSDataStoreDelegate createContextForIdentifier:parentContext:parentIdentifierPath:]
--
-- If the dataStore does not have a delegate and there are missing contexts then an incomplete list of contexts will be passed to the completion handler. Completion block is called on a background thread.
--
-- ObjC selector: @- descendantMatchingIdentifierPath:completion:@
descendantMatchingIdentifierPath_completion :: (IsCLSContext clsContext, IsNSArray identifierPath) => clsContext -> identifierPath -> Ptr () -> IO ()
descendantMatchingIdentifierPath_completion clsContext  identifierPath completion =
withObjCPtr identifierPath $ \raw_identifierPath ->
    sendMsg clsContext (mkSelector "descendantMatchingIdentifierPath:completion:") retVoid [argPtr (castPtr raw_identifierPath :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Adds a child context to specify the user can navigate to the child from this context.
--
-- Used only for presentation purpose. Unlike
--
-- -[CLSContext addChildContext:]
--
-- , this method does not affect the identifierPath.
--
-- ObjC selector: @- addNavigationChildContext:@
addNavigationChildContext :: (IsCLSContext clsContext, IsCLSContext child) => clsContext -> child -> IO ()
addNavigationChildContext clsContext  child =
withObjCPtr child $ \raw_child ->
    sendMsg clsContext (mkSelector "addNavigationChildContext:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | Removes the navigation path to the child context from this context.
--
-- Used only for presentation purpose. Unlike
--
-- -[CLSContext removeFromParent:]
--
-- , this method does not affect the identiferPath.
--
-- ObjC selector: @- removeNavigationChildContext:@
removeNavigationChildContext :: (IsCLSContext clsContext, IsCLSContext child) => clsContext -> child -> IO ()
removeNavigationChildContext clsContext  child =
withObjCPtr child $ \raw_child ->
    sendMsg clsContext (mkSelector "removeNavigationChildContext:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | Context identifier path of this context.
--
-- The identifier path starts with the main app context object and finishes with the identifier of this context. This is the identifier path that one would use in
--
-- -[CLSDataStore contextsMatchingIdintifierPath:completion:]
--
-- to find `this' context.
--
-- ObjC selector: @- identifierPath@
identifierPath :: IsCLSContext clsContext => clsContext -> IO (Id NSArray)
identifierPath clsContext  =
  sendMsg clsContext (mkSelector "identifierPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | App-assigned identifier. This identifier should work across users and devices and be unique with regards to its siblings within its parent.
--
-- The identifier could be used to embed information later used for deep linking. For example: /hydrogen-element,/ or /chapter-1./
--
-- ObjC selector: @- identifier@
identifier :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
identifier clsContext  =
  sendMsg clsContext (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Alternative deep link URL using universal links.
--
-- If your app supports universal links, you can supply them here to link the content this context represents.
--
-- ObjC selector: @- universalLinkURL@
universalLinkURL :: IsCLSContext clsContext => clsContext -> IO (Id NSURL)
universalLinkURL clsContext  =
  sendMsg clsContext (mkSelector "universalLinkURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Alternative deep link URL using universal links.
--
-- If your app supports universal links, you can supply them here to link the content this context represents.
--
-- ObjC selector: @- setUniversalLinkURL:@
setUniversalLinkURL :: (IsCLSContext clsContext, IsNSURL value) => clsContext -> value -> IO ()
setUniversalLinkURL clsContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg clsContext (mkSelector "setUniversalLinkURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Type of this context
--
-- The type that best describes this context.
--
-- ObjC selector: @- type@
type_ :: IsCLSContext clsContext => clsContext -> IO CLSContextType
type_ clsContext  =
  fmap (coerce :: CLong -> CLSContextType) $ sendMsg clsContext (mkSelector "type") retCLong []

-- | An optional user-visible name for the context if its type is CLSContextTypeCustom.
--
-- This property is relevant only if the type is CLSContextTypeCustom. This string should be localized. If this property is not set for a context of type CLSContextTypeCustom, Schoolwork app will use a default localized string ‘Custom’ as the name of the activity representing this context.
--
-- ObjC selector: @- customTypeName@
customTypeName :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
customTypeName clsContext  =
  sendMsg clsContext (mkSelector "customTypeName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional user-visible name for the context if its type is CLSContextTypeCustom.
--
-- This property is relevant only if the type is CLSContextTypeCustom. This string should be localized. If this property is not set for a context of type CLSContextTypeCustom, Schoolwork app will use a default localized string ‘Custom’ as the name of the activity representing this context.
--
-- ObjC selector: @- setCustomTypeName:@
setCustomTypeName :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setCustomTypeName clsContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg clsContext (mkSelector "setCustomTypeName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Title of this context.
--
-- For example: /Level/ 1 /./
--
-- ObjC selector: @- title@
title :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
title clsContext  =
  sendMsg clsContext (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Title of this context.
--
-- For example: /Level/ 1 /./
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setTitle clsContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg clsContext (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The displayOrder is by default sorted ascending.
--
-- Set the displayOrder if you want your contexts to be displayed in a particular order. The sort key is used as a way to sort sibling contexts in a particular order.
--
-- ObjC selector: @- displayOrder@
displayOrder :: IsCLSContext clsContext => clsContext -> IO CLong
displayOrder clsContext  =
  sendMsg clsContext (mkSelector "displayOrder") retCLong []

-- | The displayOrder is by default sorted ascending.
--
-- Set the displayOrder if you want your contexts to be displayed in a particular order. The sort key is used as a way to sort sibling contexts in a particular order.
--
-- ObjC selector: @- setDisplayOrder:@
setDisplayOrder :: IsCLSContext clsContext => clsContext -> CLong -> IO ()
setDisplayOrder clsContext  value =
  sendMsg clsContext (mkSelector "setDisplayOrder:") retVoid [argCLong (fromIntegral value)]

-- | Topic associated with this context.
--
-- See above for valid, predefined topics.
--
-- ObjC selector: @- topic@
topic :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
topic clsContext  =
  sendMsg clsContext (mkSelector "topic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Topic associated with this context.
--
-- See above for valid, predefined topics.
--
-- ObjC selector: @- setTopic:@
setTopic :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setTopic clsContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg clsContext (mkSelector "setTopic:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This property is true if the context can be assigned as an activity.
--
-- The default value of this property is true. This should be set to false for a context that is used as a container for other contexts, but by itself, is not an assignable activity.
--
-- ObjC selector: @- assignable@
assignable :: IsCLSContext clsContext => clsContext -> IO Bool
assignable clsContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clsContext (mkSelector "assignable") retCULong []

-- | This property is true if the context can be assigned as an activity.
--
-- The default value of this property is true. This should be set to false for a context that is used as a container for other contexts, but by itself, is not an assignable activity.
--
-- ObjC selector: @- setAssignable:@
setAssignable :: IsCLSContext clsContext => clsContext -> Bool -> IO ()
setAssignable clsContext  value =
  sendMsg clsContext (mkSelector "setAssignable:") retVoid [argCULong (if value then 1 else 0)]

-- | Suggested age range of students, expressed in years, for whom this context is suitable. This information is intended to help teachers to choose age-appropriate activities for their students.
--
-- The default value is [0, NSIntegerMax - 1]. This is /NSRange(0...Int.max-1)/ in /Swift/ or /NSMakeRange(0,NSIntegerMax)/ in /Objective-C./ Set the lower bound to 0 to specify no minimum age limit and set the upper bound to NSIntegerMax - 1 to specify no maximum age limit.
--
-- An age range of 4 to 6 years is expressed by @em NSRange(4...6) in @em Swift or by @em NSMakeRange(4,3) in @Objective-C.
--
-- An age range of up 10 years is expressed by @em NSRange(0...10) in @em Swift or by @em NSMakeRange(0,11) in @Objective-C.
--
-- An age range of 18 years or above is expressed by @em NSRange(18...Int.max-1) in @em Swift or by @em NSMakeRange(18,NSIntegerMax-18) in @Objective-C.
--
-- ObjC selector: @- suggestedAge@
suggestedAge :: IsCLSContext clsContext => clsContext -> IO NSRange
suggestedAge clsContext  =
  sendMsgStret clsContext (mkSelector "suggestedAge") retNSRange []

-- | Suggested age range of students, expressed in years, for whom this context is suitable. This information is intended to help teachers to choose age-appropriate activities for their students.
--
-- The default value is [0, NSIntegerMax - 1]. This is /NSRange(0...Int.max-1)/ in /Swift/ or /NSMakeRange(0,NSIntegerMax)/ in /Objective-C./ Set the lower bound to 0 to specify no minimum age limit and set the upper bound to NSIntegerMax - 1 to specify no maximum age limit.
--
-- An age range of 4 to 6 years is expressed by @em NSRange(4...6) in @em Swift or by @em NSMakeRange(4,3) in @Objective-C.
--
-- An age range of up 10 years is expressed by @em NSRange(0...10) in @em Swift or by @em NSMakeRange(0,11) in @Objective-C.
--
-- An age range of 18 years or above is expressed by @em NSRange(18...Int.max-1) in @em Swift or by @em NSMakeRange(18,NSIntegerMax-18) in @Objective-C.
--
-- ObjC selector: @- setSuggestedAge:@
setSuggestedAge :: IsCLSContext clsContext => clsContext -> NSRange -> IO ()
setSuggestedAge clsContext  value =
  sendMsg clsContext (mkSelector "setSuggestedAge:") retVoid [argNSRange value]

-- | Suggested time range, expressed in minutes, to complete the activity. This information will help teachers as they choose activities for their students.
--
-- The default value is [0, NSIntegerMax - 1]. This is /NSRange(0...Int.max-1)/ in /Swift/ or /NSMakeRange(0,NSIntegerMax)/ in /Objective-C./ Set the lower bound value to 0 to specify no minimum time limit and set the upper bound to NSIntegerMax - 1 to specify no maximum time limit.
--
-- An time range of 10 to 15 minutes is expressed by @em NSRange(10...15) in @em Swift or by @em NSMakeRange(10,6) in @Objective-C.
--
-- An time range of up to 10 minutes is expressed by @em NSRange(0...10) in @em Swift or by @em NSMakeRange(0,11) in @Objective-C.
--
-- An time range of at least 20 minutes is expressed by @em NSRange(20...Int.max-1) in @em Swift or by @em NSMakeRange(20,NSIntegerMax-20) in @Objective-C.
--
-- ObjC selector: @- suggestedCompletionTime@
suggestedCompletionTime :: IsCLSContext clsContext => clsContext -> IO NSRange
suggestedCompletionTime clsContext  =
  sendMsgStret clsContext (mkSelector "suggestedCompletionTime") retNSRange []

-- | Suggested time range, expressed in minutes, to complete the activity. This information will help teachers as they choose activities for their students.
--
-- The default value is [0, NSIntegerMax - 1]. This is /NSRange(0...Int.max-1)/ in /Swift/ or /NSMakeRange(0,NSIntegerMax)/ in /Objective-C./ Set the lower bound value to 0 to specify no minimum time limit and set the upper bound to NSIntegerMax - 1 to specify no maximum time limit.
--
-- An time range of 10 to 15 minutes is expressed by @em NSRange(10...15) in @em Swift or by @em NSMakeRange(10,6) in @Objective-C.
--
-- An time range of up to 10 minutes is expressed by @em NSRange(0...10) in @em Swift or by @em NSMakeRange(0,11) in @Objective-C.
--
-- An time range of at least 20 minutes is expressed by @em NSRange(20...Int.max-1) in @em Swift or by @em NSMakeRange(20,NSIntegerMax-20) in @Objective-C.
--
-- ObjC selector: @- setSuggestedCompletionTime:@
setSuggestedCompletionTime :: IsCLSContext clsContext => clsContext -> NSRange -> IO ()
setSuggestedCompletionTime clsContext  value =
  sendMsg clsContext (mkSelector "setSuggestedCompletionTime:") retVoid [argNSRange value]

-- | Specifies progress reporting capablities of the app for this context.
--
-- This information is intended to help teachers as they choose activities for their students. By default a CLSContext will have one CLSProgressReportingCapability instance of kind CLSProgressReportingCapabilityKindDuration. More progress reporting capabilities can be specified via '-addProgressReportingCapabilities:' to customize this set.
--
-- ObjC selector: @- progressReportingCapabilities@
progressReportingCapabilities :: IsCLSContext clsContext => clsContext -> IO (Id NSSet)
progressReportingCapabilities clsContext  =
  sendMsg clsContext (mkSelector "progressReportingCapabilities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional user-visible summary describing the context limited to 4000 characters in length.
--
-- This may be used to provide information about the types of activities available under a given context or the context itself. This string should be localized.
--
-- ObjC selector: @- summary@
summary :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
summary clsContext  =
  sendMsg clsContext (mkSelector "summary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional user-visible summary describing the context limited to 4000 characters in length.
--
-- This may be used to provide information about the types of activities available under a given context or the context itself. This string should be localized.
--
-- ObjC selector: @- setSummary:@
setSummary :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setSummary clsContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg clsContext (mkSelector "setSummary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An optional thumbnail image associated with the context.
--
-- The size of this image should be equal to or larger than 80x80 pixels and equal to or smaller than 330x330 pixels. Images larger than 330x330 pixels will be scaled down. Images with both dimensions smaller than 80x80 pixels will not be accepted.
--
-- ObjC selector: @- thumbnail@
thumbnail :: IsCLSContext clsContext => clsContext -> IO (Ptr ())
thumbnail clsContext  =
  fmap castPtr $ sendMsg clsContext (mkSelector "thumbnail") (retPtr retVoid) []

-- | An optional thumbnail image associated with the context.
--
-- The size of this image should be equal to or larger than 80x80 pixels and equal to or smaller than 330x330 pixels. Images larger than 330x330 pixels will be scaled down. Images with both dimensions smaller than 80x80 pixels will not be accepted.
--
-- ObjC selector: @- setThumbnail:@
setThumbnail :: IsCLSContext clsContext => clsContext -> Ptr () -> IO ()
setThumbnail clsContext  value =
  sendMsg clsContext (mkSelector "setThumbnail:") retVoid [argPtr value]

-- | Returns true if self is the active context.
--
-- ObjC selector: @- active@
active :: IsCLSContext clsContext => clsContext -> IO Bool
active clsContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clsContext (mkSelector "active") retCULong []

-- | Returns the current activity.
--
-- Activity associated with a context.  If no activity was ever created this is nil. See: @-[CLSContext@ createNewActivity]; for more details.
--
-- ObjC selector: @- currentActivity@
currentActivity :: IsCLSContext clsContext => clsContext -> IO (Id CLSActivity)
currentActivity clsContext  =
  sendMsg clsContext (mkSelector "currentActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the parent of this context.
--
-- ObjC selector: @- parent@
parent :: IsCLSContext clsContext => clsContext -> IO (Id CLSContext)
parent clsContext  =
  sendMsg clsContext (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Child contexts that can be navigated to from this context.
--
-- Returns all the child contexts added via
--
-- -[CLSContext addNavigationChildContext:]
--
-- ObjC selector: @- navigationChildContexts@
navigationChildContexts :: IsCLSContext clsContext => clsContext -> IO (Id NSArray)
navigationChildContexts clsContext  =
  sendMsg clsContext (mkSelector "navigationChildContexts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:identifier:title:@
initWithType_identifier_titleSelector :: Selector
initWithType_identifier_titleSelector = mkSelector "initWithType:identifier:title:"

-- | @Selector@ for @becomeActive@
becomeActiveSelector :: Selector
becomeActiveSelector = mkSelector "becomeActive"

-- | @Selector@ for @resignActive@
resignActiveSelector :: Selector
resignActiveSelector = mkSelector "resignActive"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @addProgressReportingCapabilities:@
addProgressReportingCapabilitiesSelector :: Selector
addProgressReportingCapabilitiesSelector = mkSelector "addProgressReportingCapabilities:"

-- | @Selector@ for @resetProgressReportingCapabilities@
resetProgressReportingCapabilitiesSelector :: Selector
resetProgressReportingCapabilitiesSelector = mkSelector "resetProgressReportingCapabilities"

-- | @Selector@ for @createNewActivity@
createNewActivitySelector :: Selector
createNewActivitySelector = mkSelector "createNewActivity"

-- | @Selector@ for @removeFromParent@
removeFromParentSelector :: Selector
removeFromParentSelector = mkSelector "removeFromParent"

-- | @Selector@ for @addChildContext:@
addChildContextSelector :: Selector
addChildContextSelector = mkSelector "addChildContext:"

-- | @Selector@ for @descendantMatchingIdentifierPath:completion:@
descendantMatchingIdentifierPath_completionSelector :: Selector
descendantMatchingIdentifierPath_completionSelector = mkSelector "descendantMatchingIdentifierPath:completion:"

-- | @Selector@ for @addNavigationChildContext:@
addNavigationChildContextSelector :: Selector
addNavigationChildContextSelector = mkSelector "addNavigationChildContext:"

-- | @Selector@ for @removeNavigationChildContext:@
removeNavigationChildContextSelector :: Selector
removeNavigationChildContextSelector = mkSelector "removeNavigationChildContext:"

-- | @Selector@ for @identifierPath@
identifierPathSelector :: Selector
identifierPathSelector = mkSelector "identifierPath"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @universalLinkURL@
universalLinkURLSelector :: Selector
universalLinkURLSelector = mkSelector "universalLinkURL"

-- | @Selector@ for @setUniversalLinkURL:@
setUniversalLinkURLSelector :: Selector
setUniversalLinkURLSelector = mkSelector "setUniversalLinkURL:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @customTypeName@
customTypeNameSelector :: Selector
customTypeNameSelector = mkSelector "customTypeName"

-- | @Selector@ for @setCustomTypeName:@
setCustomTypeNameSelector :: Selector
setCustomTypeNameSelector = mkSelector "setCustomTypeName:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @displayOrder@
displayOrderSelector :: Selector
displayOrderSelector = mkSelector "displayOrder"

-- | @Selector@ for @setDisplayOrder:@
setDisplayOrderSelector :: Selector
setDisplayOrderSelector = mkSelector "setDisplayOrder:"

-- | @Selector@ for @topic@
topicSelector :: Selector
topicSelector = mkSelector "topic"

-- | @Selector@ for @setTopic:@
setTopicSelector :: Selector
setTopicSelector = mkSelector "setTopic:"

-- | @Selector@ for @assignable@
assignableSelector :: Selector
assignableSelector = mkSelector "assignable"

-- | @Selector@ for @setAssignable:@
setAssignableSelector :: Selector
setAssignableSelector = mkSelector "setAssignable:"

-- | @Selector@ for @suggestedAge@
suggestedAgeSelector :: Selector
suggestedAgeSelector = mkSelector "suggestedAge"

-- | @Selector@ for @setSuggestedAge:@
setSuggestedAgeSelector :: Selector
setSuggestedAgeSelector = mkSelector "setSuggestedAge:"

-- | @Selector@ for @suggestedCompletionTime@
suggestedCompletionTimeSelector :: Selector
suggestedCompletionTimeSelector = mkSelector "suggestedCompletionTime"

-- | @Selector@ for @setSuggestedCompletionTime:@
setSuggestedCompletionTimeSelector :: Selector
setSuggestedCompletionTimeSelector = mkSelector "setSuggestedCompletionTime:"

-- | @Selector@ for @progressReportingCapabilities@
progressReportingCapabilitiesSelector :: Selector
progressReportingCapabilitiesSelector = mkSelector "progressReportingCapabilities"

-- | @Selector@ for @summary@
summarySelector :: Selector
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @thumbnail@
thumbnailSelector :: Selector
thumbnailSelector = mkSelector "thumbnail"

-- | @Selector@ for @setThumbnail:@
setThumbnailSelector :: Selector
setThumbnailSelector = mkSelector "setThumbnail:"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @currentActivity@
currentActivitySelector :: Selector
currentActivitySelector = mkSelector "currentActivity"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

-- | @Selector@ for @navigationChildContexts@
navigationChildContextsSelector :: Selector
navigationChildContextsSelector = mkSelector "navigationChildContexts"


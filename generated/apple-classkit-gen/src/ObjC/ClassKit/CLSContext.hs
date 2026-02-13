{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activeSelector
  , addChildContextSelector
  , addNavigationChildContextSelector
  , addProgressReportingCapabilitiesSelector
  , assignableSelector
  , becomeActiveSelector
  , createNewActivitySelector
  , currentActivitySelector
  , customTypeNameSelector
  , descendantMatchingIdentifierPath_completionSelector
  , displayOrderSelector
  , identifierPathSelector
  , identifierSelector
  , initSelector
  , initWithType_identifier_titleSelector
  , navigationChildContextsSelector
  , newSelector
  , parentSelector
  , progressReportingCapabilitiesSelector
  , removeFromParentSelector
  , removeNavigationChildContextSelector
  , resetProgressReportingCapabilitiesSelector
  , resignActiveSelector
  , setAssignableSelector
  , setCustomTypeNameSelector
  , setDisplayOrderSelector
  , setSuggestedAgeSelector
  , setSuggestedCompletionTimeSelector
  , setSummarySelector
  , setThumbnailSelector
  , setTitleSelector
  , setTopicSelector
  , setTypeSelector
  , setUniversalLinkURLSelector
  , suggestedAgeSelector
  , suggestedCompletionTimeSelector
  , summarySelector
  , thumbnailSelector
  , titleSelector
  , topicSelector
  , typeSelector
  , universalLinkURLSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsCLSContext clsContext => clsContext -> IO (Id CLSContext)
init_ clsContext =
  sendOwnedMessage clsContext initSelector

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
initWithType_identifier_title clsContext type_ identifier title =
  sendOwnedMessage clsContext initWithType_identifier_titleSelector type_ (toNSString identifier) (toNSString title)

-- | Marks contexts as active.
--
-- If a context is already active, it will remain active. If another context is active, the other will resign active before this one becomes active.
--
-- ObjC selector: @- becomeActive@
becomeActive :: IsCLSContext clsContext => clsContext -> IO ()
becomeActive clsContext =
  sendMessage clsContext becomeActiveSelector

-- | Resign being active.
--
-- This method does nothing if the reciever of the message is not active.
--
-- ObjC selector: @- resignActive@
resignActive :: IsCLSContext clsContext => clsContext -> IO ()
resignActive clsContext =
  sendMessage clsContext resignActiveSelector

-- | Sets the type.
--
-- Use this to update a previously saved context.
--
-- ObjC selector: @- setType:@
setType :: IsCLSContext clsContext => clsContext -> CLSContextType -> IO ()
setType clsContext type_ =
  sendMessage clsContext setTypeSelector type_

-- | Add or replace additional progress reporting capabilities of the app for this context.
--
-- If this parameter contains multiple items with the same value for kind, then one of them will be arbitrarily selected and used. If this parameter contains a capability of kind CLSProgressReportingCapabilityKindDuration, it will be ignored.
--
-- @capabilities@ — Progress reporting capabilities to add or replace existing capabilties.
--
-- ObjC selector: @- addProgressReportingCapabilities:@
addProgressReportingCapabilities :: (IsCLSContext clsContext, IsNSSet capabilities) => clsContext -> capabilities -> IO ()
addProgressReportingCapabilities clsContext capabilities =
  sendMessage clsContext addProgressReportingCapabilitiesSelector (toNSSet capabilities)

-- | Clears CLSProgressReportingCapability objects added to the receiver.
--
-- Removes all capabilities added via '-addProgressReportingCapabilities:'. The context will have the default progress reporting capability of kind CLSProgressReportingCapabilityKindDuration.
--
-- ObjC selector: @- resetProgressReportingCapabilities@
resetProgressReportingCapabilities :: IsCLSContext clsContext => clsContext -> IO ()
resetProgressReportingCapabilities clsContext =
  sendMessage clsContext resetProgressReportingCapabilitiesSelector

-- | Creates a new activity
--
-- Creates a new activity and sets it as the current activity.
--
-- ObjC selector: @- createNewActivity@
createNewActivity :: IsCLSContext clsContext => clsContext -> IO (Id CLSActivity)
createNewActivity clsContext =
  sendMessage clsContext createNewActivitySelector

-- | Removes this child context from its parent.
--
-- If you remove a context from its parent and do not add it as a child of another context, it will be deleted when you call -save on the dataStore.
--
-- ObjC selector: @- removeFromParent@
removeFromParent :: IsCLSContext clsContext => clsContext -> IO ()
removeFromParent clsContext =
  sendMessage clsContext removeFromParentSelector

-- | Adds a child context.
--
-- A context can only have a single parent.
--
-- Note: objectID of child context may change after it's been added.
--
-- ObjC selector: @- addChildContext:@
addChildContext :: (IsCLSContext clsContext, IsCLSContext child) => clsContext -> child -> IO ()
addChildContext clsContext child =
  sendMessage clsContext addChildContextSelector (toCLSContext child)

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
descendantMatchingIdentifierPath_completion clsContext identifierPath completion =
  sendMessage clsContext descendantMatchingIdentifierPath_completionSelector (toNSArray identifierPath) completion

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
addNavigationChildContext clsContext child =
  sendMessage clsContext addNavigationChildContextSelector (toCLSContext child)

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
removeNavigationChildContext clsContext child =
  sendMessage clsContext removeNavigationChildContextSelector (toCLSContext child)

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
identifierPath clsContext =
  sendMessage clsContext identifierPathSelector

-- | App-assigned identifier. This identifier should work across users and devices and be unique with regards to its siblings within its parent.
--
-- The identifier could be used to embed information later used for deep linking. For example: /hydrogen-element,/ or /chapter-1./
--
-- ObjC selector: @- identifier@
identifier :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
identifier clsContext =
  sendMessage clsContext identifierSelector

-- | Alternative deep link URL using universal links.
--
-- If your app supports universal links, you can supply them here to link the content this context represents.
--
-- ObjC selector: @- universalLinkURL@
universalLinkURL :: IsCLSContext clsContext => clsContext -> IO (Id NSURL)
universalLinkURL clsContext =
  sendMessage clsContext universalLinkURLSelector

-- | Alternative deep link URL using universal links.
--
-- If your app supports universal links, you can supply them here to link the content this context represents.
--
-- ObjC selector: @- setUniversalLinkURL:@
setUniversalLinkURL :: (IsCLSContext clsContext, IsNSURL value) => clsContext -> value -> IO ()
setUniversalLinkURL clsContext value =
  sendMessage clsContext setUniversalLinkURLSelector (toNSURL value)

-- | Type of this context
--
-- The type that best describes this context.
--
-- ObjC selector: @- type@
type_ :: IsCLSContext clsContext => clsContext -> IO CLSContextType
type_ clsContext =
  sendMessage clsContext typeSelector

-- | An optional user-visible name for the context if its type is CLSContextTypeCustom.
--
-- This property is relevant only if the type is CLSContextTypeCustom. This string should be localized. If this property is not set for a context of type CLSContextTypeCustom, Schoolwork app will use a default localized string ‘Custom’ as the name of the activity representing this context.
--
-- ObjC selector: @- customTypeName@
customTypeName :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
customTypeName clsContext =
  sendMessage clsContext customTypeNameSelector

-- | An optional user-visible name for the context if its type is CLSContextTypeCustom.
--
-- This property is relevant only if the type is CLSContextTypeCustom. This string should be localized. If this property is not set for a context of type CLSContextTypeCustom, Schoolwork app will use a default localized string ‘Custom’ as the name of the activity representing this context.
--
-- ObjC selector: @- setCustomTypeName:@
setCustomTypeName :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setCustomTypeName clsContext value =
  sendMessage clsContext setCustomTypeNameSelector (toNSString value)

-- | Title of this context.
--
-- For example: /Level/ 1 /./
--
-- ObjC selector: @- title@
title :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
title clsContext =
  sendMessage clsContext titleSelector

-- | Title of this context.
--
-- For example: /Level/ 1 /./
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setTitle clsContext value =
  sendMessage clsContext setTitleSelector (toNSString value)

-- | The displayOrder is by default sorted ascending.
--
-- Set the displayOrder if you want your contexts to be displayed in a particular order. The sort key is used as a way to sort sibling contexts in a particular order.
--
-- ObjC selector: @- displayOrder@
displayOrder :: IsCLSContext clsContext => clsContext -> IO CLong
displayOrder clsContext =
  sendMessage clsContext displayOrderSelector

-- | The displayOrder is by default sorted ascending.
--
-- Set the displayOrder if you want your contexts to be displayed in a particular order. The sort key is used as a way to sort sibling contexts in a particular order.
--
-- ObjC selector: @- setDisplayOrder:@
setDisplayOrder :: IsCLSContext clsContext => clsContext -> CLong -> IO ()
setDisplayOrder clsContext value =
  sendMessage clsContext setDisplayOrderSelector value

-- | Topic associated with this context.
--
-- See above for valid, predefined topics.
--
-- ObjC selector: @- topic@
topic :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
topic clsContext =
  sendMessage clsContext topicSelector

-- | Topic associated with this context.
--
-- See above for valid, predefined topics.
--
-- ObjC selector: @- setTopic:@
setTopic :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setTopic clsContext value =
  sendMessage clsContext setTopicSelector (toNSString value)

-- | This property is true if the context can be assigned as an activity.
--
-- The default value of this property is true. This should be set to false for a context that is used as a container for other contexts, but by itself, is not an assignable activity.
--
-- ObjC selector: @- assignable@
assignable :: IsCLSContext clsContext => clsContext -> IO Bool
assignable clsContext =
  sendMessage clsContext assignableSelector

-- | This property is true if the context can be assigned as an activity.
--
-- The default value of this property is true. This should be set to false for a context that is used as a container for other contexts, but by itself, is not an assignable activity.
--
-- ObjC selector: @- setAssignable:@
setAssignable :: IsCLSContext clsContext => clsContext -> Bool -> IO ()
setAssignable clsContext value =
  sendMessage clsContext setAssignableSelector value

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
suggestedAge clsContext =
  sendMessage clsContext suggestedAgeSelector

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
setSuggestedAge clsContext value =
  sendMessage clsContext setSuggestedAgeSelector value

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
suggestedCompletionTime clsContext =
  sendMessage clsContext suggestedCompletionTimeSelector

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
setSuggestedCompletionTime clsContext value =
  sendMessage clsContext setSuggestedCompletionTimeSelector value

-- | Specifies progress reporting capablities of the app for this context.
--
-- This information is intended to help teachers as they choose activities for their students. By default a CLSContext will have one CLSProgressReportingCapability instance of kind CLSProgressReportingCapabilityKindDuration. More progress reporting capabilities can be specified via '-addProgressReportingCapabilities:' to customize this set.
--
-- ObjC selector: @- progressReportingCapabilities@
progressReportingCapabilities :: IsCLSContext clsContext => clsContext -> IO (Id NSSet)
progressReportingCapabilities clsContext =
  sendMessage clsContext progressReportingCapabilitiesSelector

-- | An optional user-visible summary describing the context limited to 4000 characters in length.
--
-- This may be used to provide information about the types of activities available under a given context or the context itself. This string should be localized.
--
-- ObjC selector: @- summary@
summary :: IsCLSContext clsContext => clsContext -> IO (Id NSString)
summary clsContext =
  sendMessage clsContext summarySelector

-- | An optional user-visible summary describing the context limited to 4000 characters in length.
--
-- This may be used to provide information about the types of activities available under a given context or the context itself. This string should be localized.
--
-- ObjC selector: @- setSummary:@
setSummary :: (IsCLSContext clsContext, IsNSString value) => clsContext -> value -> IO ()
setSummary clsContext value =
  sendMessage clsContext setSummarySelector (toNSString value)

-- | An optional thumbnail image associated with the context.
--
-- The size of this image should be equal to or larger than 80x80 pixels and equal to or smaller than 330x330 pixels. Images larger than 330x330 pixels will be scaled down. Images with both dimensions smaller than 80x80 pixels will not be accepted.
--
-- ObjC selector: @- thumbnail@
thumbnail :: IsCLSContext clsContext => clsContext -> IO (Ptr ())
thumbnail clsContext =
  sendMessage clsContext thumbnailSelector

-- | An optional thumbnail image associated with the context.
--
-- The size of this image should be equal to or larger than 80x80 pixels and equal to or smaller than 330x330 pixels. Images larger than 330x330 pixels will be scaled down. Images with both dimensions smaller than 80x80 pixels will not be accepted.
--
-- ObjC selector: @- setThumbnail:@
setThumbnail :: IsCLSContext clsContext => clsContext -> Ptr () -> IO ()
setThumbnail clsContext value =
  sendMessage clsContext setThumbnailSelector value

-- | Returns true if self is the active context.
--
-- ObjC selector: @- active@
active :: IsCLSContext clsContext => clsContext -> IO Bool
active clsContext =
  sendMessage clsContext activeSelector

-- | Returns the current activity.
--
-- Activity associated with a context.  If no activity was ever created this is nil. See: @-[CLSContext@ createNewActivity]; for more details.
--
-- ObjC selector: @- currentActivity@
currentActivity :: IsCLSContext clsContext => clsContext -> IO (Id CLSActivity)
currentActivity clsContext =
  sendMessage clsContext currentActivitySelector

-- | Returns the parent of this context.
--
-- ObjC selector: @- parent@
parent :: IsCLSContext clsContext => clsContext -> IO (Id CLSContext)
parent clsContext =
  sendMessage clsContext parentSelector

-- | Child contexts that can be navigated to from this context.
--
-- Returns all the child contexts added via
--
-- -[CLSContext addNavigationChildContext:]
--
-- ObjC selector: @- navigationChildContexts@
navigationChildContexts :: IsCLSContext clsContext => clsContext -> IO (Id NSArray)
navigationChildContexts clsContext =
  sendMessage clsContext navigationChildContextsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLSContext)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLSContext)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithType:identifier:title:@
initWithType_identifier_titleSelector :: Selector '[CLSContextType, Id NSString, Id NSString] (Id CLSContext)
initWithType_identifier_titleSelector = mkSelector "initWithType:identifier:title:"

-- | @Selector@ for @becomeActive@
becomeActiveSelector :: Selector '[] ()
becomeActiveSelector = mkSelector "becomeActive"

-- | @Selector@ for @resignActive@
resignActiveSelector :: Selector '[] ()
resignActiveSelector = mkSelector "resignActive"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[CLSContextType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @addProgressReportingCapabilities:@
addProgressReportingCapabilitiesSelector :: Selector '[Id NSSet] ()
addProgressReportingCapabilitiesSelector = mkSelector "addProgressReportingCapabilities:"

-- | @Selector@ for @resetProgressReportingCapabilities@
resetProgressReportingCapabilitiesSelector :: Selector '[] ()
resetProgressReportingCapabilitiesSelector = mkSelector "resetProgressReportingCapabilities"

-- | @Selector@ for @createNewActivity@
createNewActivitySelector :: Selector '[] (Id CLSActivity)
createNewActivitySelector = mkSelector "createNewActivity"

-- | @Selector@ for @removeFromParent@
removeFromParentSelector :: Selector '[] ()
removeFromParentSelector = mkSelector "removeFromParent"

-- | @Selector@ for @addChildContext:@
addChildContextSelector :: Selector '[Id CLSContext] ()
addChildContextSelector = mkSelector "addChildContext:"

-- | @Selector@ for @descendantMatchingIdentifierPath:completion:@
descendantMatchingIdentifierPath_completionSelector :: Selector '[Id NSArray, Ptr ()] ()
descendantMatchingIdentifierPath_completionSelector = mkSelector "descendantMatchingIdentifierPath:completion:"

-- | @Selector@ for @addNavigationChildContext:@
addNavigationChildContextSelector :: Selector '[Id CLSContext] ()
addNavigationChildContextSelector = mkSelector "addNavigationChildContext:"

-- | @Selector@ for @removeNavigationChildContext:@
removeNavigationChildContextSelector :: Selector '[Id CLSContext] ()
removeNavigationChildContextSelector = mkSelector "removeNavigationChildContext:"

-- | @Selector@ for @identifierPath@
identifierPathSelector :: Selector '[] (Id NSArray)
identifierPathSelector = mkSelector "identifierPath"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @universalLinkURL@
universalLinkURLSelector :: Selector '[] (Id NSURL)
universalLinkURLSelector = mkSelector "universalLinkURL"

-- | @Selector@ for @setUniversalLinkURL:@
setUniversalLinkURLSelector :: Selector '[Id NSURL] ()
setUniversalLinkURLSelector = mkSelector "setUniversalLinkURL:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CLSContextType
typeSelector = mkSelector "type"

-- | @Selector@ for @customTypeName@
customTypeNameSelector :: Selector '[] (Id NSString)
customTypeNameSelector = mkSelector "customTypeName"

-- | @Selector@ for @setCustomTypeName:@
setCustomTypeNameSelector :: Selector '[Id NSString] ()
setCustomTypeNameSelector = mkSelector "setCustomTypeName:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @displayOrder@
displayOrderSelector :: Selector '[] CLong
displayOrderSelector = mkSelector "displayOrder"

-- | @Selector@ for @setDisplayOrder:@
setDisplayOrderSelector :: Selector '[CLong] ()
setDisplayOrderSelector = mkSelector "setDisplayOrder:"

-- | @Selector@ for @topic@
topicSelector :: Selector '[] (Id NSString)
topicSelector = mkSelector "topic"

-- | @Selector@ for @setTopic:@
setTopicSelector :: Selector '[Id NSString] ()
setTopicSelector = mkSelector "setTopic:"

-- | @Selector@ for @assignable@
assignableSelector :: Selector '[] Bool
assignableSelector = mkSelector "assignable"

-- | @Selector@ for @setAssignable:@
setAssignableSelector :: Selector '[Bool] ()
setAssignableSelector = mkSelector "setAssignable:"

-- | @Selector@ for @suggestedAge@
suggestedAgeSelector :: Selector '[] NSRange
suggestedAgeSelector = mkSelector "suggestedAge"

-- | @Selector@ for @setSuggestedAge:@
setSuggestedAgeSelector :: Selector '[NSRange] ()
setSuggestedAgeSelector = mkSelector "setSuggestedAge:"

-- | @Selector@ for @suggestedCompletionTime@
suggestedCompletionTimeSelector :: Selector '[] NSRange
suggestedCompletionTimeSelector = mkSelector "suggestedCompletionTime"

-- | @Selector@ for @setSuggestedCompletionTime:@
setSuggestedCompletionTimeSelector :: Selector '[NSRange] ()
setSuggestedCompletionTimeSelector = mkSelector "setSuggestedCompletionTime:"

-- | @Selector@ for @progressReportingCapabilities@
progressReportingCapabilitiesSelector :: Selector '[] (Id NSSet)
progressReportingCapabilitiesSelector = mkSelector "progressReportingCapabilities"

-- | @Selector@ for @summary@
summarySelector :: Selector '[] (Id NSString)
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector '[Id NSString] ()
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @thumbnail@
thumbnailSelector :: Selector '[] (Ptr ())
thumbnailSelector = mkSelector "thumbnail"

-- | @Selector@ for @setThumbnail:@
setThumbnailSelector :: Selector '[Ptr ()] ()
setThumbnailSelector = mkSelector "setThumbnail:"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @currentActivity@
currentActivitySelector :: Selector '[] (Id CLSActivity)
currentActivitySelector = mkSelector "currentActivity"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id CLSContext)
parentSelector = mkSelector "parent"

-- | @Selector@ for @navigationChildContexts@
navigationChildContextsSelector :: Selector '[] (Id NSArray)
navigationChildContextsSelector = mkSelector "navigationChildContexts"


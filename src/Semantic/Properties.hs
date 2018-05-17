{-# LANGUAGE TypeApplications #-}
module Semantic.Properties where

import GHC.Generics

import Control.Arrow ((&&&))

import Pure.Data.View (JSV)
import Pure.Data.Txt (Txt)
import Pure.Data.Default (Default(..))

import qualified Data.Proxy as P

import Data.Function ((&))

import Prelude hiding (Floating)

class HasProp p a where
    type Prop p a :: *
    getProp :: p -> a -> Prop p a
    setProp :: p -> Prop p a -> a -> a

pattern Is :: forall p a. (HasProp p a, Prop p a ~ Bool) => p -> a -> a
pattern Is p a <- (getProp (undefined :: p) &&& id &&& (const (undefined :: p)) -> (True,(a,p))) where
    Is p a = setProp p True a

pattern Not :: forall p a. (HasProp p a, Prop p a ~ Bool) => p -> a -> a
pattern Not p a <- (getProp (undefined :: p) &&& id &&& (const (undefined :: p)) -> (False,(a,p))) where
    Not p a = setProp p False a

data Action = Action_
pattern Action :: HasProp Action a => Prop Action a -> a -> a
pattern Action p a <- (getProp Action_ &&& id -> (p,a)) where
    Action p a = setProp Action_ p a

data Active = Active_
pattern Active :: HasProp Active a => Prop Active a -> a -> a
pattern Active p a <- (getProp Active_ &&& id -> (p,a)) where
    Active p a = setProp Active_ p a

data Aligned = Aligned_
pattern Aligned :: HasProp Aligned a => Prop Aligned a -> a -> a
pattern Aligned p a <- (getProp Aligned_ &&& id -> (p,a)) where
    Aligned p a = setProp Aligned_ p a

data Animated = Animated_
pattern Animated :: HasProp Animated a => Prop Animated a -> a -> a
pattern Animated p a <- (getProp Animated_ &&& id -> (p,a)) where
    Animated p a = setProp Animated_ p a

data Animation = Animation_
pattern Animation :: HasProp Animation a => Prop Animation a -> a -> a
pattern Animation p a <- (getProp Animation_ &&& id -> (p,a)) where
    Animation p a = setProp Animation_ p a

data AnimationDuration
    = Uniform Int
    | Skewed
        { hide :: Int
        , show :: Int
        }
    deriving (Generic,Default,Ord,Eq)

pattern AnimationDuration :: HasProp AnimationDuration a => Prop AnimationDuration a -> a -> a
pattern AnimationDuration p a <- (getProp (undefined :: AnimationDuration) &&& id -> (p,a)) where
    AnimationDuration p a = setProp (undefined :: AnimationDuration) p a

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

-- instance HasProp As (View) where
--     type Prop As (View) = Features -> [View] -> View

--     -- Note: For Managed, Component, Text, Null, and Raw views, this
--     -- method just extracts a reinjector that ignores its arguments.
--     -- For keyed nodes, KHTML and KSVG, this method will create a
--     -- `Constructor (View)` that adds indexes with (zip [0..]).
--     getProp _ v = (As_,r)
--       where
--         r = case v of
--                 HTMLView _ t _ _    -> mkHTML t
--                 KHTMLView _ t _ _ _ -> \fs cs -> list (mkHTML t) fs (zip [0..] cs)
--                 SVGView _ t _ _     -> mkSVG t
--                 KSVGView _ t _ _ _  -> \fs cs -> list (mkSVG t) fs (zip [0..] cs)
--                 v                   -> (\_ _ -> v)

--     -- Note: For Managed, Coponent, Text, Null, and Raw views, this
--     -- method ignores its `as` argument and returns the view as is.
--     setProp _ as v =
--         case v of
--             HTMLView _ _ fs cs    -> as fs cs
--             KHTMLView _ _ fs cs _ -> list as fs cs
--             SVGView _ _ fs cs     -> as fs cs
--             KSVGView _ _ fs cs _  -> list as fs cs
--             _                     -> v

data AspectRatio = AspectRatio_
pattern AspectRatio :: HasProp AspectRatio a => Prop AspectRatio a -> a -> a
pattern AspectRatio p a <- (getProp AspectRatio_ &&& id -> (p,a)) where
    AspectRatio p a = setProp AspectRatio_ p a

data Attached = Attached_
pattern Attached :: HasProp Attached a => Prop Attached a -> a -> a
pattern Attached p a <- (getProp Attached_ &&& id -> (p,a)) where
    Attached p a = setProp Attached_ p a

data AutoHeight = AutoHeight_
pattern AutoHeight :: HasProp AutoHeight a => Prop AutoHeight a -> a -> a
pattern AutoHeight p a <- (getProp AutoHeight_ &&& id -> (p,a)) where
    AutoHeight p a = setProp AutoHeight_ p a

data Autoplay = Autoplay_
pattern Autoplay :: HasProp Autoplay a => Prop Autoplay a -> a -> a
pattern Autoplay p a <- (getProp Autoplay_ &&& id -> (p,a)) where
    Autoplay p a = setProp Autoplay_ p a

data AutoSuccess = AutoSuccess_
pattern AutoSuccess :: HasProp AutoSuccess a => Prop AutoSuccess a -> a -> a
pattern AutoSuccess p a <- (getProp AutoSuccess_ &&& id -> (p,a)) where
    AutoSuccess p a = setProp AutoSuccess_ p a

data Avatar = Avatar_
pattern Avatar :: HasProp Avatar a => Prop Avatar a -> a -> a
pattern Avatar p a <- (getProp Avatar_ &&& id -> (p,a)) where
    Avatar p a = setProp Avatar_ p a

data Basic = Basic_
pattern Basic :: HasProp Basic a => Prop Basic a -> a -> a
pattern Basic p a <- (getProp Basic_ &&& id -> (p,a)) where
    Basic p a = setProp Basic_ p a

data Block = Block_
pattern Block :: HasProp Block a => Prop Block a -> a -> a
pattern Block p a <- (getProp Block_ &&& id -> (p,a)) where
    Block p a = setProp Block_ p a

data Blurring = Blurring_
pattern Blurring :: HasProp Blurring a => Prop Blurring a -> a -> a
pattern Blurring p a <- (getProp Blurring_ &&& id -> (p,a)) where
    Blurring p a = setProp Blurring_ p a

data Bordered = Bordered_
pattern Bordered :: HasProp Bordered a => Prop Bordered a -> a -> a
pattern Bordered p a <- (getProp Bordered_ &&& id -> (p,a)) where
    Bordered p a = setProp Bordered_ p a

data Borderless = Borderless_
pattern Borderless :: HasProp Borderless a => Prop Borderless a -> a -> a
pattern Borderless p a <- (getProp Borderless_ &&& id -> (p,a)) where
    Borderless p a = setProp Borderless_ p a

data BottomOffset = BottomOffset_
pattern BottomOffset :: HasProp BottomOffset a => Prop BottomOffset a -> a -> a
pattern BottomOffset p a <- (getProp BottomOffset_ &&& id -> (p,a)) where
    BottomOffset p a = setProp BottomOffset_ p a

data Branded = Branded_
pattern Branded :: HasProp Branded a => Prop Branded a -> a -> a
pattern Branded p a <- (getProp Branded_ &&& id -> (p,a)) where
    Branded p a = setProp Branded_ p a

data Bulleted = Bulleted_
pattern Bulleted :: HasProp Bulleted a => Prop Bulleted a -> a -> a
pattern Bulleted p a <- (getProp Bulleted_ &&& id -> (p,a)) where
    Bulleted p a = setProp Bulleted_ p a

data CancelButton = CancelButton_
pattern CancelButton :: HasProp CancelButton a => Prop CancelButton a -> a -> a
pattern CancelButton p a <- (getProp CancelButton_ &&& id -> (p,a)) where
    CancelButton p a = setProp CancelButton_ p a

data Category = Category_
pattern Category :: HasProp Category a => Prop Category a -> a -> a
pattern Category p a <- (getProp Category_ &&& id -> (p,a)) where
    Category p a = setProp Category_ p a

data Celled = Celled_
pattern Celled :: HasProp Celled a => Prop Celled a -> a -> a
pattern Celled p a <- (getProp Celled_ &&& id -> (p,a)) where
    Celled p a = setProp Celled_ p a

data Centered = Centered_
pattern Centered :: HasProp Centered a => Prop Centered a -> a -> a
pattern Centered p a <- (getProp Centered_ &&& id -> (p,a)) where
    Centered p a = setProp Centered_ p a

data Circular = Circular_
pattern Circular :: HasProp Circular a => Prop Circular a -> a -> a
pattern Circular p a <- (getProp Circular_ &&& id -> (p,a)) where
    Circular p a = setProp Circular_ p a

data Clearable = Clearable_
pattern Clearable :: HasProp Clearable a => Prop Clearable a -> a -> a
pattern Clearable p a <- (getProp Clearable_ &&& id -> (p,a)) where
    Clearable p a = setProp Clearable_ p a

data Clearing = Clearing_
pattern Clearing :: HasProp Clearing a => Prop Clearing a -> a -> a
pattern Clearing p a <- (getProp Clearing_ &&& id -> (p,a)) where
    Clearing p a = setProp Clearing_ p a

data Close = Close_
pattern Close :: HasProp Close a => Prop Close a -> a -> a
pattern Close p a <- (getProp Close_ &&& id -> (p,a)) where
    Close p a = setProp Close_ p a

data CloseOnDimmerClick = CloseOnDimmerClick_
pattern CloseOnDimmerClick :: HasProp CloseOnDimmerClick a => Prop CloseOnDimmerClick a -> a -> a
pattern CloseOnDimmerClick p a <- (getProp CloseOnDimmerClick_ &&& id -> (p,a)) where
    CloseOnDimmerClick p a = setProp CloseOnDimmerClick_ p a

data CloseOnDocumentClick = CloseOnDocumentClick_
pattern CloseOnDocumentClick :: HasProp CloseOnDocumentClick a => Prop CloseOnDocumentClick a -> a -> a
pattern CloseOnDocumentClick p a <- (getProp CloseOnDocumentClick_ &&& id -> (p,a)) where
    CloseOnDocumentClick p a = setProp CloseOnDocumentClick_ p a

data CloseOnEscape = CloseOnEscape_
pattern CloseOnEscape :: HasProp CloseOnEscape a => Prop CloseOnEscape a -> a -> a
pattern CloseOnEscape p a <- (getProp CloseOnEscape_ &&& id -> (p,a)) where
    CloseOnEscape p a = setProp CloseOnEscape_ p a

data CloseOnPortalMouseLeave = CloseOnPortalMouseLeave_
pattern CloseOnPortalMouseLeave :: HasProp CloseOnPortalMouseLeave a => Prop CloseOnPortalMouseLeave a -> a -> a
pattern CloseOnPortalMouseLeave p a <- (getProp CloseOnPortalMouseLeave_ &&& id -> (p,a)) where
    CloseOnPortalMouseLeave p a = setProp CloseOnPortalMouseLeave_ p a

data CloseOnRootNodeClick = CloseOnRootNodeClick_
pattern CloseOnRootNodeClick :: HasProp CloseOnRootNodeClick a => Prop CloseOnRootNodeClick a -> a -> a
pattern CloseOnRootNodeClick p a <- (getProp CloseOnRootNodeClick_ &&& id -> (p,a)) where
    CloseOnRootNodeClick p a = setProp CloseOnRootNodeClick_ p a

data CloseOnTriggerBlur = CloseOnTriggerBlur_
pattern CloseOnTriggerBlur :: HasProp CloseOnTriggerBlur a => Prop CloseOnTriggerBlur a -> a -> a
pattern CloseOnTriggerBlur p a <- (getProp CloseOnTriggerBlur_ &&& id -> (p,a)) where
    CloseOnTriggerBlur p a = setProp CloseOnTriggerBlur_ p a

data CloseOnTriggerClick = CloseOnTriggerClick_
pattern CloseOnTriggerClick :: HasProp CloseOnTriggerClick a => Prop CloseOnTriggerClick a -> a -> a
pattern CloseOnTriggerClick p a <- (getProp CloseOnTriggerClick_ &&& id -> (p,a)) where
    CloseOnTriggerClick p a = setProp CloseOnTriggerClick_ p a

data CloseOnTriggerMouseLeave = CloseOnTriggerMouseLeave_
pattern CloseOnTriggerMouseLeave :: HasProp CloseOnTriggerMouseLeave a => Prop CloseOnTriggerMouseLeave a -> a -> a
pattern CloseOnTriggerMouseLeave p a <- (getProp CloseOnTriggerMouseLeave_ &&& id -> (p,a)) where
    CloseOnTriggerMouseLeave p a = setProp CloseOnTriggerMouseLeave_ p a

data Collapsed = Collapsed_
pattern Collapsed :: HasProp Collapsed a => Prop Collapsed a -> a -> a
pattern Collapsed p a <- (getProp Collapsed_ &&& id -> (p,a)) where
    Collapsed p a = setProp Collapsed_ p a

data Collapsing = Collapsing_
pattern Collapsing :: HasProp Collapsing a => Prop Collapsing a -> a -> a
pattern Collapsing p a <- (getProp Collapsing_ &&& id -> (p,a)) where
    Collapsing p a = setProp Collapsing_ p a

data Color = Color_
pattern Color :: HasProp Color a => Prop Color a -> a -> a
pattern Color p a <- (getProp Color_ &&& id -> (p,a)) where
    Color p a = setProp Color_ p a

data Columns = Columns_
pattern Columns :: HasProp Columns a => Prop Columns a -> a -> a
pattern Columns p a <- (getProp Columns_ &&& id -> (p,a)) where
    Columns p a = setProp Columns_ p a

data Compact = Compact_
pattern Compact :: HasProp Compact a => Prop Compact a -> a -> a
pattern Compact p a <- (getProp Compact_ &&& id -> (p,a)) where
    Compact p a = setProp Compact_ p a

data Completed = Completed_
pattern Completed :: HasProp Completed a => Prop Completed a -> a -> a
pattern Completed p a <- (getProp Completed_ &&& id -> (p,a)) where
    Completed p a = setProp Completed_ p a

data ConfirmButton = ConfirmButton_
pattern ConfirmButton :: HasProp ConfirmButton a => Prop ConfirmButton a -> a -> a
pattern ConfirmButton p a <- (getProp ConfirmButton_ &&& id -> (p,a)) where
    ConfirmButton p a = setProp ConfirmButton_ p a

data Context = Context_
pattern Context :: HasProp Context a => Prop Context a -> a -> a
pattern Context p a <- (getProp Context_ &&& id -> (p,a)) where
    Context p a = setProp Context_ p a

data Continuous = Continuous_
pattern Continuous :: HasProp Continuous a => Prop Continuous a -> a -> a
pattern Continuous p a <- (getProp Continuous_ &&& id -> (p,a)) where
    Continuous p a = setProp Continuous_ p a

data Corner = Corner_
pattern Corner :: HasProp Corner a => Prop Corner a -> a -> a
pattern Corner p a <- (getProp Corner_ &&& id -> (p,a)) where
    Corner p a = setProp Corner_ p a

data CurrentRating = CurrentRating_
pattern CurrentRating :: HasProp CurrentRating a => Prop CurrentRating a -> a -> a
pattern CurrentRating p a <- (getProp CurrentRating_ &&& id -> (p,a)) where
    CurrentRating p a = setProp CurrentRating_ p a

data DefaultActive = DefaultActive_
pattern DefaultActive :: HasProp DefaultActive a => Prop DefaultActive a -> a -> a
pattern DefaultActive p a <- (getProp DefaultActive_ &&& id -> (p,a)) where
    DefaultActive p a = setProp DefaultActive_ p a

data DefaultOpen = DefaultOpen_
pattern DefaultOpen :: HasProp DefaultOpen a => Prop DefaultOpen a -> a -> a
pattern DefaultOpen p a <- (getProp DefaultOpen_ &&& id -> (p,a)) where
    DefaultOpen p a = setProp DefaultOpen_ p a

data DefaultRating = DefaultRating_
pattern DefaultRating :: HasProp DefaultRating a => Prop DefaultRating a -> a -> a
pattern DefaultRating p a <- (getProp DefaultRating_ &&& id -> (p,a)) where
    DefaultRating p a = setProp DefaultRating_ p a

data Definition = Definition_
pattern Definition :: HasProp Definition a => Prop Definition a -> a -> a
pattern Definition p a <- (getProp Definition_ &&& id -> (p,a)) where
    Definition p a = setProp Definition_ p a

data Dimmed = Dimmed_
pattern Dimmed :: HasProp Dimmed a => Prop Dimmed a -> a -> a
pattern Dimmed p a <- (getProp Dimmed_ &&& id -> (p,a)) where
    Dimmed p a = setProp Dimmed_ p a

data DimmerType = DimmerType_
pattern DimmerType :: HasProp DimmerType a => Prop DimmerType a -> a -> a
pattern DimmerType p a <- (getProp DimmerType_ &&& id -> (p,a)) where
    DimmerType p a = setProp DimmerType_ p a

data Direction = Direction_
pattern Direction :: HasProp Direction a => Prop Direction a -> a -> a
pattern Direction p a <- (getProp Direction_ &&& id -> (p,a)) where
    Direction p a = setProp Direction_ p a

data Disabled = Disabled_
pattern Disabled :: HasProp Disabled a => Prop Disabled a -> a -> a
pattern Disabled p a <- (getProp Disabled_ &&& id -> (p,a)) where
    Disabled p a = setProp Disabled_ p a

data Divided = Divided_
pattern Divided :: HasProp Divided a => Prop Divided a -> a -> a
pattern Divided p a <- (getProp Divided_ &&& id -> (p,a)) where
    Divided p a = setProp Divided_ p a

data Dividing = Dividing_
pattern Dividing :: HasProp Dividing a => Prop Dividing a -> a -> a
pattern Dividing p a <- (getProp Dividing_ &&& id -> (p,a)) where
    Dividing p a = setProp Dividing_ p a

data Doubling = Doubling_
pattern Doubling :: HasProp Doubling a => Prop Doubling a -> a -> a
pattern Doubling p a <- (getProp Doubling_ &&& id -> (p,a)) where
    Doubling p a = setProp Doubling_ p a

data Empty = Empty_
pattern Empty :: HasProp Empty a => Prop Empty a -> a -> a
pattern Empty p a <- (getProp Empty_ &&& id -> (p,a)) where
    Empty p a = setProp Empty_ p a

data Error = Error_
pattern Error :: HasProp Error a => Prop Error a -> a -> a
pattern Error p a <- (getProp Error_ &&& id -> (p,a)) where
    Error p a = setProp Error_ p a

data Extra = Extra_
pattern Extra :: HasProp Extra a => Prop Extra a -> a -> a
pattern Extra p a <- (getProp Extra_ &&& id -> (p,a)) where
    Extra p a = setProp Extra_ p a

data FireOnMount = FireOnMount_
pattern FireOnMount :: HasProp FireOnMount a => Prop FireOnMount a -> a -> a
pattern FireOnMount p a <- (getProp FireOnMount_ &&& id -> (p,a)) where
    FireOnMount p a = setProp FireOnMount_ p a

data Fitted = Fitted_
pattern Fitted :: HasProp Fitted a => Prop Fitted a -> a -> a
pattern Fitted p a <- (getProp Fitted_ &&& id -> (p,a)) where
    Fitted p a = setProp Fitted_ p a

data Fixed = Fixed_
pattern Fixed :: HasProp Fixed a => Prop Fixed a -> a -> a
pattern Fixed p a <- (getProp Fixed_ &&& id -> (p,a)) where
    Fixed p a = setProp Fixed_ p a

data Flipped = Flipped_
pattern Flipped :: HasProp Flipped a => Prop Flipped a -> a -> a
pattern Flipped p a <- (getProp Flipped_ &&& id -> (p,a)) where
    Flipped p a = setProp Flipped_ p a

data Floated = Floated_
pattern Floated :: HasProp Floated a => Prop Floated a -> a -> a
pattern Floated p a <- (getProp Floated_ &&& id -> (p,a)) where
    Floated p a = setProp Floated_ p a

data Floating = Floating_
pattern Floating :: HasProp Floating a => Prop Floating a -> a -> a
pattern Floating p a <- (getProp Floating_ &&& id -> (p,a)) where
    Floating p a = setProp Floating_ p a

data Flowing = Flowing_
pattern Flowing :: HasProp Flowing a => Prop Flowing a -> a -> a
pattern Flowing p a <- (getProp Flowing_ &&& id -> (p,a)) where
    Flowing p a = setProp Flowing_ p a

data Fluid = Fluid_
pattern Fluid :: HasProp Fluid a => Prop Fluid a -> a -> a
pattern Fluid p a <- (getProp Fluid_ &&& id -> (p,a)) where
    Fluid p a = setProp Fluid_ p a

data Focus = Focus_
pattern Focus :: HasProp Focus a => Prop Focus a -> a -> a
pattern Focus p a <- (getProp Focus_ &&& id -> (p,a)) where
    Focus p a = setProp Focus_ p a

data Focused = Focused_
pattern Focused :: HasProp Focused a => Prop Focused a -> a -> a
pattern Focused p a <- (getProp Focused_ &&& id -> (p,a)) where
    Focused p a = setProp Focused_ p a

data FullWidth = FullWidth_
pattern FullWidth :: HasProp FullWidth a => Prop FullWidth a -> a -> a
pattern FullWidth p a <- (getProp FullWidth_ &&& id -> (p,a)) where
    FullWidth p a = setProp FullWidth_ p a

data Grouped = Grouped_
pattern Grouped :: HasProp Grouped a => Prop Grouped a -> a -> a
pattern Grouped p a <- (getProp Grouped_ &&& id -> (p,a)) where
    Grouped p a = setProp Grouped_ p a

data Hidden = Hidden_
pattern Hidden :: HasProp Hidden a => Prop Hidden a -> a -> a
pattern Hidden p a <- (getProp Hidden_ &&& id -> (p,a)) where
    Hidden p a = setProp Hidden_ p a

data HideOnScroll = HideOnScroll_
pattern HideOnScroll :: HasProp HideOnScroll a => Prop HideOnScroll a -> a -> a
pattern HideOnScroll p a <- (getProp HideOnScroll_ &&& id -> (p,a)) where
    HideOnScroll p a = setProp HideOnScroll_ p a

data Horizontal = Horizontal_
pattern Horizontal :: HasProp Horizontal a => Prop Horizontal a -> a -> a
pattern Horizontal p a <- (getProp Horizontal_ &&& id -> (p,a)) where
    Horizontal p a = setProp Horizontal_ p a

data Hoverable = Hoverable_
pattern Hoverable :: HasProp Hoverable a => Prop Hoverable a -> a -> a
pattern Hoverable p a <- (getProp Hoverable_ &&& id -> (p,a)) where
    Hoverable p a = setProp Hoverable_ p a

data Index = Index_
pattern Index :: HasProp Index a => Prop Index a -> a -> a
pattern Index p a <- (getProp Index_ &&& id -> (p,a)) where
    Index p a = setProp Index_ p a

data Indicating = Indicating_
pattern Indicating :: HasProp Indicating a => Prop Indicating a -> a -> a
pattern Indicating p a <- (getProp Indicating_ &&& id -> (p,a)) where
    Indicating p a = setProp Indicating_ p a

data Info = Info_
pattern Info :: HasProp Info a => Prop Info a -> a -> a
pattern Info p a <- (getProp Info_ &&& id -> (p,a)) where
    Info p a = setProp Info_ p a

data Inline = Inline_
pattern Inline :: HasProp Inline a => Prop Inline a -> a -> a
pattern Inline p a <- (getProp Inline_ &&& id -> (p,a)) where
    Inline p a = setProp Inline_ p a

data InnerRef = InnerRef_
pattern InnerRef :: HasProp InnerRef a => Prop InnerRef a -> a -> a
pattern InnerRef p a <- (getProp InnerRef_ &&& id -> (p,a)) where
    InnerRef p a = setProp InnerRef_ p a

data InputRef = InputRef_
pattern InputRef :: HasProp InputRef a => Prop InputRef a -> a -> a
pattern InputRef p a <- (getProp InputRef_ &&& id -> (p,a)) where
    InputRef p a = setProp InputRef_ p a

data Instant = Instant_
pattern Instant :: HasProp Instant a => Prop Instant a -> a -> a
pattern Instant p a <- (getProp Instant_ &&& id -> (p,a)) where
    Instant p a = setProp Instant_ p a

data Internal = Internal_
pattern Internal :: HasProp Internal a => Prop Internal a -> a -> a
pattern Internal p a <- (getProp Internal_ &&& id -> (p,a)) where
    Internal p a = setProp Internal_ p a

data Inverted = Inverted_
pattern Inverted :: HasProp Inverted a => Prop Inverted a -> a -> a
pattern Inverted p a <- (getProp Inverted_ &&& id -> (p,a)) where
    Inverted p a = setProp Inverted_ p a

data IsButton = IsButton_
pattern IsButton :: HasProp IsButton a => Prop IsButton a -> a -> a
pattern IsButton p a <- (getProp IsButton_ &&& id -> (p,a)) where
    IsButton p a = setProp IsButton_ p a

data Checked = Checked_
pattern Checked :: HasProp Checked a => Prop Checked a -> a -> a
pattern Checked p a <- (getProp Checked_ &&& id -> (p,a)) where
    Checked p a = setProp Checked_ p a

data IsContainer = IsContainer_
pattern IsContainer :: HasProp IsContainer a => Prop IsContainer a -> a -> a
pattern IsContainer p a <- (getProp IsContainer_ &&& id -> (p,a)) where
    IsContainer p a = setProp IsContainer_ p a

data IsHeader = IsHeader_
pattern IsHeader :: HasProp IsHeader a => Prop IsHeader a -> a -> a
pattern IsHeader p a <- (getProp IsHeader_ &&& id -> (p,a)) where
    IsHeader p a = setProp IsHeader_ p a

data IsIcon = IsIcon_
pattern IsIcon :: HasProp IsIcon a => Prop IsIcon a -> a -> a
pattern IsIcon p a <- (getProp IsIcon_ &&& id -> (p,a)) where
    IsIcon p a = setProp IsIcon_ p a

data IsImage = IsImage_
pattern IsImage :: HasProp IsImage a => Prop IsImage a -> a -> a
pattern IsImage p a <- (getProp IsImage_ &&& id -> (p,a)) where
    IsImage p a = setProp IsImage_ p a

data IsIndeterminate = IsIndeterminate_
pattern IsIndeterminate :: HasProp IsIndeterminate a => Prop IsIndeterminate a -> a -> a
pattern IsIndeterminate p a <- (getProp IsIndeterminate_ &&& id -> (p,a)) where
    IsIndeterminate p a = setProp IsIndeterminate_ p a

data IsItem = IsItem_
pattern IsItem :: HasProp IsItem a => Prop IsItem a -> a -> a
pattern IsItem p a <- (getProp IsItem_ &&& id -> (p,a)) where
    IsItem p a = setProp IsItem_ p a

data IsRadio = IsRadio_
pattern IsRadio :: HasProp IsRadio a => Prop IsRadio a -> a -> a
pattern IsRadio p a <- (getProp IsRadio_ &&& id -> (p,a)) where
    IsRadio p a = setProp IsRadio_ p a

data IsSearch = IsSearch_
pattern IsSearch :: HasProp IsSearch a => Prop IsSearch a -> a -> a
pattern IsSearch p a <- (getProp IsSearch_ &&& id -> (p,a)) where
    IsSearch p a = setProp IsSearch_ p a

data IsText = IsText_
pattern IsText :: HasProp IsText a => Prop IsText a -> a -> a
pattern IsText p a <- (getProp IsText_ &&& id -> (p,a)) where
    IsText p a = setProp IsText_ p a

data ItemsPerRow = ItemsPerRow_
pattern ItemsPerRow :: HasProp ItemsPerRow a => Prop ItemsPerRow a -> a -> a
pattern ItemsPerRow p a <- (getProp ItemsPerRow_ &&& id -> (p,a)) where
    ItemsPerRow p a = setProp ItemsPerRow_ p a

data Labeled = Labeled_
pattern Labeled :: HasProp Labeled a => Prop Labeled a -> a -> a
pattern Labeled p a <- (getProp Labeled_ &&& id -> (p,a)) where
    Labeled p a = setProp Labeled_ p a

data LabelPosition = LabelPosition_
pattern LabelPosition :: HasProp LabelPosition a => Prop LabelPosition a -> a -> a
pattern LabelPosition p a <- (getProp LabelPosition_ &&& id -> (p,a)) where
    LabelPosition p a = setProp LabelPosition_ p a

data Link = Link_
pattern Link :: HasProp Link a => Prop Link a -> a -> a
pattern Link p a <- (getProp Link_ &&& id -> (p,a)) where
    Link p a = setProp Link_ p a

data Loading = Loading_
pattern Loading :: HasProp Loading a => Prop Loading a -> a -> a
pattern Loading p a <- (getProp Loading_ &&& id -> (p,a)) where
    Loading p a = setProp Loading_ p a

data Localize = Localize_
pattern Localize :: HasProp Localize a => Prop Localize a -> a -> a
pattern Localize p a <- (getProp Localize_ &&& id -> (p,a)) where
    Localize p a = setProp Localize_ p a

data MaxRating = MaxRating_
pattern MaxRating :: HasProp MaxRating a => Prop MaxRating a -> a -> a
pattern MaxRating p a <- (getProp MaxRating_ &&& id -> (p,a)) where
    MaxRating p a = setProp MaxRating_ p a

data MaxWidth = MaxWidth_
pattern MaxWidth :: HasProp MaxWidth a => Prop MaxWidth a -> a -> a
pattern MaxWidth p a <- (getProp MaxWidth_ &&& id -> (p,a)) where
    MaxWidth p a = setProp MaxWidth_ p a

data Minimal = Minimal_
pattern Minimal :: HasProp Minimal a => Prop Minimal a -> a -> a
pattern Minimal p a <- (getProp Minimal_ &&& id -> (p,a)) where
    Minimal p a = setProp Minimal_ p a

data MinWidth = MinWidth_
pattern MinWidth :: HasProp MinWidth a => Prop MinWidth a -> a -> a
pattern MinWidth p a <- (getProp MinWidth_ &&& id -> (p,a)) where
    MinWidth p a = setProp MinWidth_ p a

data MountNode = MountNode_
pattern MountNode :: HasProp MountNode a => Prop MountNode a -> a -> a
pattern MountNode p a <- (getProp MountNode_ &&& id -> (p,a)) where
    MountNode p a = setProp MountNode_ p a

data MountOnShow = MountOnShow_
pattern MountOnShow :: HasProp MountOnShow a => Prop MountOnShow a -> a -> a
pattern MountOnShow p a <- (getProp MountOnShow_ &&& id -> (p,a)) where
    MountOnShow p a = setProp MountOnShow_ p a

data MouseEnterDelay = MouseEnterDelay_
pattern MouseEnterDelay :: HasProp MouseEnterDelay a => Prop MouseEnterDelay a -> a -> a
pattern MouseEnterDelay p a <- (getProp MouseEnterDelay_ &&& id -> (p,a)) where
    MouseEnterDelay p a = setProp MouseEnterDelay_ p a

data MouseLeaveDelay = MouseLeaveDelay_
pattern MouseLeaveDelay :: HasProp MouseLeaveDelay a => Prop MouseLeaveDelay a -> a -> a
pattern MouseLeaveDelay p a <- (getProp MouseLeaveDelay_ &&& id -> (p,a)) where
    MouseLeaveDelay p a = setProp MouseLeaveDelay_ p a

data Multiple = Multiple_
pattern Multiple :: HasProp Multiple a => Prop Multiple a -> a -> a
pattern Multiple p a <- (getProp Multiple_ &&& id -> (p,a)) where
    Multiple p a = setProp Multiple_ p a

data Name = Name_
pattern Name :: HasProp Name a => Prop Name a -> a -> a
pattern Name p a <- (getProp Name_ &&& id -> (p,a)) where
    Name p a = setProp Name_ p a

data Negative = Negative_
pattern Negative :: HasProp Negative a => Prop Negative a -> a -> a
pattern Negative p a <- (getProp Negative_ &&& id -> (p,a)) where
    Negative p a = setProp Negative_ p a

data Offset = Offset_
pattern Offset :: HasProp Offset a => Prop Offset a -> a -> a
pattern Offset p a <- (getProp Offset_ &&& id -> (p,a)) where
    Offset p a = setProp Offset_ p a

data OnBlur = OnBlur_
pattern OnBlur :: HasProp OnBlur a => Prop OnBlur a -> a -> a
pattern OnBlur p a <- (getProp OnBlur_ &&& id -> (p,a)) where
    OnBlur p a = setProp OnBlur_ p a

data OnBottom = OnBottom_
pattern OnBottom :: HasProp OnBottom a => Prop OnBottom a -> a -> a
pattern OnBottom p a <- (getProp OnBottom_ &&& id -> (p,a)) where
    OnBottom p a = setProp OnBottom_ p a

data OnBottomPassed = OnBottomPassed_
pattern OnBottomPassed :: HasProp OnBottomPassed a => Prop OnBottomPassed a -> a -> a
pattern OnBottomPassed p a <- (getProp OnBottomPassed_ &&& id -> (p,a)) where
    OnBottomPassed p a = setProp OnBottomPassed_ p a

data OnBottomPassedReverse = OnBottomPassedReverse_
pattern OnBottomPassedReverse :: HasProp OnBottomPassedReverse a => Prop OnBottomPassedReverse a -> a -> a
pattern OnBottomPassedReverse p a <- (getProp OnBottomPassedReverse_ &&& id -> (p,a)) where
    OnBottomPassedReverse p a = setProp OnBottomPassedReverse_ p a

data OnBottomVisible = OnBottomVisible_
pattern OnBottomVisible :: HasProp OnBottomVisible a => Prop OnBottomVisible a -> a -> a
pattern OnBottomVisible p a <- (getProp OnBottomVisible_ &&& id -> (p,a)) where
    OnBottomVisible p a = setProp OnBottomVisible_ p a

data OnBottomVisibleReverse = OnBottomVisibleReverse_
pattern OnBottomVisibleReverse :: HasProp OnBottomVisibleReverse a => Prop OnBottomVisibleReverse a -> a -> a
pattern OnBottomVisibleReverse p a <- (getProp OnBottomVisibleReverse_ &&& id -> (p,a)) where
    OnBottomVisibleReverse p a = setProp OnBottomVisibleReverse_ p a

data OnCancel = OnCancel_
pattern OnCancel :: HasProp OnCancel a => Prop OnCancel a -> a -> a
pattern OnCancel p a <- (getProp OnCancel_ &&& id -> (p,a)) where
    OnCancel p a = setProp OnCancel_ p a

data Once = Once_
pattern Once :: HasProp Once a => Prop Once a -> a -> a
pattern Once p a <- (getProp Once_ &&& id -> (p,a)) where
    Once p a = setProp Once_ p a

data OnChange = OnChange_
pattern OnChange :: HasProp OnChange a => Prop OnChange a -> a -> a
pattern OnChange p a <- (getProp OnChange_ &&& id -> (p,a)) where
    OnChange p a = setProp OnChange_ p a

data OnClick = OnClick_
pattern OnClick :: HasProp OnClick a => Prop OnClick a -> a -> a
pattern OnClick p a <- (getProp OnClick_ &&& id -> (p,a)) where
    OnClick p a = setProp OnClick_ p a

data OnClickOutside = OnClickOutside_
pattern OnClickOutside :: HasProp OnClickOutside a => Prop OnClickOutside a -> a -> a
pattern OnClickOutside p a <- (getProp OnClickOutside_ &&& id -> (p,a)) where
    OnClickOutside p a = setProp OnClickOutside_ p a

data OnClose = OnClose_
pattern OnClose :: HasProp OnClose a => Prop OnClose a -> a -> a
pattern OnClose p a <- (getProp OnClose_ &&& id -> (p,a)) where
    OnClose p a = setProp OnClose_ p a

data OnComplete = OnComplete_
pattern OnComplete :: HasProp OnComplete a => Prop OnComplete a -> a -> a
pattern OnComplete p a <- (getProp OnComplete_ &&& id -> (p,a)) where
    OnComplete p a = setProp OnComplete_ p a

data OnComputer = OnComputer_
pattern OnComputer :: HasProp OnComputer a => Prop OnComputer a -> a -> a
pattern OnComputer p a <- (getProp OnComputer_ &&& id -> (p,a)) where
    OnComputer p a = setProp OnComputer_ p a

data OnConfirm = OnConfirm_
pattern OnConfirm :: HasProp OnConfirm a => Prop OnConfirm a -> a -> a
pattern OnConfirm p a <- (getProp OnConfirm_ &&& id -> (p,a)) where
    OnConfirm p a = setProp OnConfirm_ p a

data OnDismiss = OnDismiss_
pattern OnDismiss :: HasProp OnDismiss a => Prop OnDismiss a -> a -> a
pattern OnDismiss p a <- (getProp OnDismiss_ &&& id -> (p,a)) where
    OnDismiss p a = setProp OnDismiss_ p a

data OnFocus = OnFocus_
pattern OnFocus :: HasProp OnFocus a => Prop OnFocus a -> a -> a
pattern OnFocus p a <- (getProp OnFocus_ &&& id -> (p,a)) where
    OnFocus p a = setProp OnFocus_ p a

data OnHide = OnHide_
pattern OnHide :: HasProp OnHide a => Prop OnHide a -> a -> a
pattern OnHide p a <- (getProp OnHide_ &&& id -> (p,a)) where
    OnHide p a = setProp OnHide_ p a

data OnInput = OnInput_
pattern OnInput :: HasProp OnInput a => Prop OnInput a -> a -> a
pattern OnInput p a <- (getProp OnInput_ &&& id -> (p,a)) where
    OnInput p a = setProp OnInput_ p a

data OnKeyUp = OnKeyUp_
pattern OnKeyUp :: HasProp OnKeyUp a => Prop OnKeyUp a -> a -> a
pattern OnKeyUp p a <- (getProp OnKeyUp_ &&& id -> (p,a)) where
    OnKeyUp p a = setProp OnKeyUp_ p a

data OnLargeScreen = OnLargeScreen_
pattern OnLargeScreen :: HasProp OnLargeScreen a => Prop OnLargeScreen a -> a -> a
pattern OnLargeScreen p a <- (getProp OnLargeScreen_ &&& id -> (p,a)) where
    OnLargeScreen p a = setProp OnLargeScreen_ p a

data Only = Only_
pattern Only :: HasProp Only a => Prop Only a -> a -> a
pattern Only p a <- (getProp Only_ &&& id -> (p,a)) where
    Only p a = setProp Only_ p a

data OnMobile = OnMobile_
pattern OnMobile :: HasProp OnMobile a => Prop OnMobile a -> a -> a
pattern OnMobile p a <- (getProp OnMobile_ &&& id -> (p,a)) where
    OnMobile p a = setProp OnMobile_ p a

data OnMount = OnMount_
pattern OnMount :: HasProp OnMount a => Prop OnMount a -> a -> a
pattern OnMount p a <- (getProp OnMount_ &&& id -> (p,a)) where
    OnMount p a = setProp OnMount_ p a

data OnMouseDown = OnMouseDown_
pattern OnMouseDown :: HasProp OnMouseDown a => Prop OnMouseDown a -> a -> a
pattern OnMouseDown p a <- (getProp OnMouseDown_ &&& id -> (p,a)) where
    OnMouseDown p a = setProp OnMouseDown_ p a

data OnMouseEnter = OnMouseEnter_
pattern OnMouseEnter :: HasProp OnMouseEnter a => Prop OnMouseEnter a -> a -> a
pattern OnMouseEnter p a <- (getProp OnMouseEnter_ &&& id -> (p,a)) where
    OnMouseEnter p a = setProp OnMouseEnter_ p a

data OnOffScreen = OnOffScreen_
pattern OnOffScreen :: HasProp OnOffScreen a => Prop OnOffScreen a -> a -> a
pattern OnOffScreen p a <- (getProp OnOffScreen_ &&& id -> (p,a)) where
    OnOffScreen p a = setProp OnOffScreen_ p a

data OnOnScreen = OnOnScreen_
pattern OnOnScreen :: HasProp OnOnScreen a => Prop OnOnScreen a -> a -> a
pattern OnOnScreen p a <- (getProp OnOnScreen_ &&& id -> (p,a)) where
    OnOnScreen p a = setProp OnOnScreen_ p a

data OnOpen = OnOpen_
pattern OnOpen :: HasProp OnOpen a => Prop OnOpen a -> a -> a
pattern OnOpen p a <- (getProp OnOpen_ &&& id -> (p,a)) where
    OnOpen p a = setProp OnOpen_ p a

data OnPassed = OnPassed_
pattern OnPassed :: HasProp OnPassed a => Prop OnPassed a -> a -> a
pattern OnPassed p a <- (getProp OnPassed_ &&& id -> (p,a)) where
    OnPassed p a = setProp OnPassed_ p a

data OnPassing = OnPassing_
pattern OnPassing :: HasProp OnPassing a => Prop OnPassing a -> a -> a
pattern OnPassing p a <- (getProp OnPassing_ &&& id -> (p,a)) where
    OnPassing p a = setProp OnPassing_ p a

data OnPassingReverse = OnPassingReverse_
pattern OnPassingReverse :: HasProp OnPassingReverse a => Prop OnPassingReverse a -> a -> a
pattern OnPassingReverse p a <- (getProp OnPassingReverse_ &&& id -> (p,a)) where
    OnPassingReverse p a = setProp OnPassingReverse_ p a

data OnRate = OnRate_
pattern OnRate :: HasProp OnRate a => Prop OnRate a -> a -> a
pattern OnRate p a <- (getProp OnRate_ &&& id -> (p,a)) where
    OnRate p a = setProp OnRate_ p a

data OnShow = OnShow_
pattern OnShow :: HasProp OnShow a => Prop OnShow a -> a -> a
pattern OnShow p a <- (getProp OnShow_ &&& id -> (p,a)) where
    OnShow p a = setProp OnShow_ p a

data OnStart = OnStart_
pattern OnStart :: HasProp OnStart a => Prop OnStart a -> a -> a
pattern OnStart p a <- (getProp OnStart_ &&& id -> (p,a)) where
    OnStart p a = setProp OnStart_ p a

data OnStick = OnStick_
pattern OnStick :: HasProp OnStick a => Prop OnStick a -> a -> a
pattern OnStick p a <- (getProp OnStick_ &&& id -> (p,a)) where
    OnStick p a = setProp OnStick_ p a

data OnSubmit = OnSubmit_
pattern OnSubmit :: HasProp OnSubmit a => Prop OnSubmit a -> a -> a
pattern OnSubmit p a <- (getProp OnSubmit_ &&& id -> (p,a)) where
    OnSubmit p a = setProp OnSubmit_ p a

data OnTablet = OnTablet_
pattern OnTablet :: HasProp OnTablet a => Prop OnTablet a -> a -> a
pattern OnTablet p a <- (getProp OnTablet_ &&& id -> (p,a)) where
    OnTablet p a = setProp OnTablet_ p a

data OnTop = OnTop_
pattern OnTop :: HasProp OnTop a => Prop OnTop a -> a -> a
pattern OnTop p a <- (getProp OnTop_ &&& id -> (p,a)) where
    OnTop p a = setProp OnTop_ p a

data OnTopPassed = OnTopPassed_
pattern OnTopPassed :: HasProp OnTopPassed a => Prop OnTopPassed a -> a -> a
pattern OnTopPassed p a <- (getProp OnTopPassed_ &&& id -> (p,a)) where
    OnTopPassed p a = setProp OnTopPassed_ p a

data OnTopPassedReverse = OnTopPassedReverse_
pattern OnTopPassedReverse :: HasProp OnTopPassedReverse a => Prop OnTopPassedReverse a -> a -> a
pattern OnTopPassedReverse p a <- (getProp OnTopPassedReverse_ &&& id -> (p,a)) where
    OnTopPassedReverse p a = setProp OnTopPassedReverse_ p a

data OnTopVisible = OnTopVisible_
pattern OnTopVisible :: HasProp OnTopVisible a => Prop OnTopVisible a -> a -> a
pattern OnTopVisible p a <- (getProp OnTopVisible_ &&& id -> (p,a)) where
    OnTopVisible p a = setProp OnTopVisible_ p a

data OnTopVisibleReverse = OnTopVisibleReverse_
pattern OnTopVisibleReverse :: HasProp OnTopVisibleReverse a => Prop OnTopVisibleReverse a -> a -> a
pattern OnTopVisibleReverse p a <- (getProp OnTopVisibleReverse_ &&& id -> (p,a)) where
    OnTopVisibleReverse p a = setProp OnTopVisibleReverse_ p a

data OnUnmount = OnUnmount_
pattern OnUnmount :: HasProp OnUnmount a => Prop OnUnmount a -> a -> a
pattern OnUnmount p a <- (getProp OnUnmount_ &&& id -> (p,a)) where
    OnUnmount p a = setProp OnUnmount_ p a

data OnUnstick = OnUnstick_
pattern OnUnstick :: HasProp OnUnstick a => Prop OnUnstick a -> a -> a
pattern OnUnstick p a <- (getProp OnUnstick_ &&& id -> (p,a)) where
    OnUnstick p a = setProp OnUnstick_ p a

data OnUpdate = OnUpdate_
pattern OnUpdate :: HasProp OnUpdate a => Prop OnUpdate a -> a -> a
pattern OnUpdate p a <- (getProp OnUpdate_ &&& id -> (p,a)) where
    OnUpdate p a = setProp OnUpdate_ p a

data OnWidescreen = OnWidescreen_
pattern OnWidescreen :: HasProp OnWidescreen a => Prop OnWidescreen a -> a -> a
pattern OnWidescreen p a <- (getProp OnWidescreen_ &&& id -> (p,a)) where
    OnWidescreen p a = setProp OnWidescreen_ p a

data Open = Open_
pattern Open :: HasProp Open a => Prop Open a -> a -> a
pattern Open p a <- (getProp Open_ &&& id -> (p,a)) where
    Open p a = setProp Open_ p a

data OpenOnTriggerClick = OpenOnTriggerClick_
pattern OpenOnTriggerClick :: HasProp OpenOnTriggerClick a => Prop OpenOnTriggerClick a -> a -> a
pattern OpenOnTriggerClick p a <- (getProp OpenOnTriggerClick_ &&& id -> (p,a)) where
    OpenOnTriggerClick p a = setProp OpenOnTriggerClick_ p a

data OpenOnTriggerFocus = OpenOnTriggerFocus_
pattern OpenOnTriggerFocus :: HasProp OpenOnTriggerFocus a => Prop OpenOnTriggerFocus a -> a -> a
pattern OpenOnTriggerFocus p a <- (getProp OpenOnTriggerFocus_ &&& id -> (p,a)) where
    OpenOnTriggerFocus p a = setProp OpenOnTriggerFocus_ p a

data OpenOnTriggerMouseEnter = OpenOnTriggerMouseEnter_
pattern OpenOnTriggerMouseEnter :: HasProp OpenOnTriggerMouseEnter a => Prop OpenOnTriggerMouseEnter a -> a -> a
pattern OpenOnTriggerMouseEnter p a <- (getProp OpenOnTriggerMouseEnter_ &&& id -> (p,a)) where
    OpenOnTriggerMouseEnter p a = setProp OpenOnTriggerMouseEnter_ p a

data Ordered = Ordered_
pattern Ordered :: HasProp Ordered a => Prop Ordered a -> a -> a
pattern Ordered p a <- (getProp Ordered_ &&& id -> (p,a)) where
    Ordered p a = setProp Ordered_ p a

data Padded = Padded_
pattern Padded :: HasProp Padded a => Prop Padded a -> a -> a
pattern Padded p a <- (getProp Padded_ &&& id -> (p,a)) where
    Padded p a = setProp Padded_ p a

data Page = Page_
pattern Page :: HasProp Page a => Prop Page a -> a -> a
pattern Page p a <- (getProp Page_ &&& id -> (p,a)) where
    Page p a = setProp Page_ p a

data Pagination = Pagination_
pattern Pagination :: HasProp Pagination a => Prop Pagination a -> a -> a
pattern Pagination p a <- (getProp Pagination_ &&& id -> (p,a)) where
    Pagination p a = setProp Pagination_ p a

data Percent = Percent_
pattern Percent :: HasProp Percent a => Prop Percent a -> a -> a
pattern Percent p a <- (getProp Percent_ &&& id -> (p,a)) where
    Percent p a = setProp Percent_ p a

data Piled = Piled_
pattern Piled :: HasProp Piled a => Prop Piled a -> a -> a
pattern Piled p a <- (getProp Piled_ &&& id -> (p,a)) where
    Piled p a = setProp Piled_ p a

data Placeholder = Placeholder_
pattern Placeholder :: HasProp Placeholder a => Prop Placeholder a -> a -> a
pattern Placeholder p a <- (getProp Placeholder_ &&& id -> (p,a)) where
    Placeholder p a = setProp Placeholder_ p a

data Pointing = Pointing_
pattern Pointing :: HasProp Pointing a => Prop Pointing a -> a -> a
pattern Pointing p a <- (getProp Pointing_ &&& id -> (p,a)) where
    Pointing p a = setProp Pointing_ p a

data Position = Position_
pattern Position :: HasProp Position a => Prop Position a -> a -> a
pattern Position p a <- (getProp Position_ &&& id -> (p,a)) where
    Position p a = setProp Position_ p a

data Positive = Positive_
pattern Positive :: HasProp Positive a => Prop Positive a -> a -> a
pattern Positive p a <- (getProp Positive_ &&& id -> (p,a)) where
    Positive p a = setProp Positive_ p a

data Precision = Precision_
pattern Precision :: HasProp Precision a => Prop Precision a -> a -> a
pattern Precision p a <- (getProp Precision_ &&& id -> (p,a)) where
    Precision p a = setProp Precision_ p a

data Prepend = Prepend_
pattern Prepend :: HasProp Prepend a => Prop Prepend a -> a -> a
pattern Prepend p a <- (getProp Prepend_ &&& id -> (p,a)) where
    Prepend p a = setProp Prepend_ p a

data Primary = Primary_
pattern Primary :: HasProp Primary a => Prop Primary a -> a -> a
pattern Primary p a <- (getProp Primary_ &&& id -> (p,a)) where
    Primary p a = setProp Primary_ p a

data Pushing = Pushing_
pattern Pushing :: HasProp Pushing a => Prop Pushing a -> a -> a
pattern Pushing p a <- (getProp Pushing_ &&& id -> (p,a)) where
    Pushing p a = setProp Pushing_ p a

data Raised = Raised_
pattern Raised :: HasProp Raised a => Prop Raised a -> a -> a
pattern Raised p a <- (getProp Raised_ &&& id -> (p,a)) where
    Raised p a = setProp Raised_ p a

data ReadOnly = ReadOnly_
pattern ReadOnly :: HasProp ReadOnly a => Prop ReadOnly a -> a -> a
pattern ReadOnly p a <- (getProp ReadOnly_ &&& id -> (p,a)) where
    ReadOnly p a = setProp ReadOnly_ p a

data Ref = Ref_
pattern Ref :: HasProp Ref a => Prop Ref a -> a -> a
pattern Ref p a <- (getProp Ref_ &&& id -> (p,a)) where
    Ref p a = setProp Ref_ p a

data Relaxed = Relaxed_
pattern Relaxed :: HasProp Relaxed a => Prop Relaxed a -> a -> a
pattern Relaxed p a <- (getProp Relaxed_ &&& id -> (p,a)) where
    Relaxed p a = setProp Relaxed_ p a

data Reply = Reply_
pattern Reply :: HasProp Reply a => Prop Reply a -> a -> a
pattern Reply p a <- (getProp Reply_ &&& id -> (p,a)) where
    Reply p a = setProp Reply_ p a

data Required = Required_
pattern Required :: HasProp Required a => Prop Required a -> a -> a
pattern Required p a <- (getProp Required_ &&& id -> (p,a)) where
    Required p a = setProp Required_ p a

data Reversed = Reversed_
pattern Reversed :: HasProp Reversed a => Prop Reversed a -> a -> a
pattern Reversed p a <- (getProp Reversed_ &&& id -> (p,a)) where
    Reversed p a = setProp Reversed_ p a

data Ribbon = Ribbon_
pattern Ribbon :: HasProp Ribbon a => Prop Ribbon a -> a -> a
pattern Ribbon p a <- (getProp Ribbon_ &&& id -> (p,a)) where
    Ribbon p a = setProp Ribbon_ p a

data Rotated = Rotated_
pattern Rotated :: HasProp Rotated a => Prop Rotated a -> a -> a
pattern Rotated p a <- (getProp Rotated_ &&& id -> (p,a)) where
    Rotated p a = setProp Rotated_ p a

data Rounded = Rounded_
pattern Rounded :: HasProp Rounded a => Prop Rounded a -> a -> a
pattern Rounded p a <- (getProp Rounded_ &&& id -> (p,a)) where
    Rounded p a = setProp Rounded_ p a

data Rows = Rows_
pattern Rows :: HasProp Rows a => Prop Rows a -> a -> a
pattern Rows p a <- (getProp Rows_ &&& id -> (p,a)) where
    Rows p a = setProp Rows_ p a

data Scrollable = Scrollable_
pattern Scrollable :: HasProp Scrollable a => Prop Scrollable a -> a -> a
pattern Scrollable p a <- (getProp Scrollable_ &&& id -> (p,a)) where
    Scrollable p a = setProp Scrollable_ p a

data ScrollContext = ScrollContext_
pattern ScrollContext :: HasProp ScrollContext a => Prop ScrollContext a -> a -> a
pattern ScrollContext p a <- (getProp ScrollContext_ &&& id -> (p,a)) where
    ScrollContext p a = setProp ScrollContext_ p a

data Scrolling = Scrolling_
pattern Scrolling :: HasProp Scrolling a => Prop Scrolling a -> a -> a
pattern Scrolling p a <- (getProp Scrolling_ &&& id -> (p,a)) where
    Scrolling p a = setProp Scrolling_ p a

data Secondary = Secondary_
pattern Secondary :: HasProp Secondary a => Prop Secondary a -> a -> a
pattern Secondary p a <- (getProp Secondary_ &&& id -> (p,a)) where
    Secondary p a = setProp Secondary_ p a

data Section = Section_
pattern Section :: HasProp Section a => Prop Section a -> a -> a
pattern Section p a <- (getProp Section_ &&& id -> (p,a)) where
    Section p a = setProp Section_ p a

data Selectable = Selectable_
pattern Selectable :: HasProp Selectable a => Prop Selectable a -> a -> a
pattern Selectable p a <- (getProp Selectable_ &&& id -> (p,a)) where
    Selectable p a = setProp Selectable_ p a

data Selected = Selected_
pattern Selected :: HasProp Selected a => Prop Selected a -> a -> a
pattern Selected p a <- (getProp Selected_ &&& id -> (p,a)) where
    Selected p a = setProp Selected_ p a

data Selection = Selection_
pattern Selection :: HasProp Selection a => Prop Selection a -> a -> a
pattern Selection p a <- (getProp Selection_ &&& id -> (p,a)) where
    Selection p a = setProp Selection_ p a

data ShowProgress = ShowProgress_
pattern ShowProgress :: HasProp ShowProgress a => Prop ShowProgress a -> a -> a
pattern ShowProgress p a <- (getProp ShowProgress_ &&& id -> (p,a)) where
    ShowProgress p a = setProp ShowProgress_ p a

data Simple = Simple_
pattern Simple :: HasProp Simple a => Prop Simple a -> a -> a
pattern Simple p a <- (getProp Simple_ &&& id -> (p,a)) where
    Simple p a = setProp Simple_ p a

data SingleLine = SingleLine_
pattern SingleLine :: HasProp SingleLine a => Prop SingleLine a -> a -> a
pattern SingleLine p a <- (getProp SingleLine_ &&& id -> (p,a)) where
    SingleLine p a = setProp SingleLine_ p a

data Size = Size_
pattern Size :: HasProp Size a => Prop Size a -> a -> a
pattern Size p a <- (getProp Size_ &&& id -> (p,a)) where
    Size p a = setProp Size_ p a

data Slider = Slider_
pattern Slider :: HasProp Slider a => Prop Slider a -> a -> a
pattern Slider p a <- (getProp Slider_ &&& id -> (p,a)) where
    Slider p a = setProp Slider_ p a

data Sortable = Sortable_
pattern Sortable :: HasProp Sortable a => Prop Sortable a -> a -> a
pattern Sortable p a <- (getProp Sortable_ &&& id -> (p,a)) where
    Sortable p a = setProp Sortable_ p a

data Sorted = Sorted_
pattern Sorted :: HasProp Sorted a => Prop Sorted a -> a -> a
pattern Sorted p a <- (getProp Sorted_ &&& id -> (p,a)) where
    Sorted p a = setProp Sorted_ p a

data Spaced = Spaced_
pattern Spaced :: HasProp Spaced a => Prop Spaced a -> a -> a
pattern Spaced p a <- (getProp Spaced_ &&& id -> (p,a)) where
    Spaced p a = setProp Spaced_ p a

data Src = Src_
pattern Src :: HasProp Src a => Prop Src a -> a -> a
pattern Src p a <- (getProp Src_ &&& id -> (p,a)) where
    Src p a = setProp Src_ p a

data Stackable = Stackable_
pattern Stackable :: HasProp Stackable a => Prop Stackable a -> a -> a
pattern Stackable p a <- (getProp Stackable_ &&& id -> (p,a)) where
    Stackable p a = setProp Stackable_ p a

data Stacked = Stacked_
pattern Stacked :: HasProp Stacked a => Prop Stacked a -> a -> a
pattern Stacked p a <- (getProp Stacked_ &&& id -> (p,a)) where
    Stacked p a = setProp Stacked_ p a

data Stretched = Stretched_
pattern Stretched :: HasProp Stretched a => Prop Stretched a -> a -> a
pattern Stretched p a <- (getProp Stretched_ &&& id -> (p,a)) where
    Stretched p a = setProp Stretched_ p a

data Striped = Striped_
pattern Striped :: HasProp Striped a => Prop Striped a -> a -> a
pattern Striped p a <- (getProp Striped_ &&& id -> (p,a)) where
    Striped p a = setProp Striped_ p a

data Structured = Structured_
pattern Structured :: HasProp Structured a => Prop Structured a -> a -> a
pattern Structured p a <- (getProp Structured_ &&& id -> (p,a)) where
    Structured p a = setProp Structured_ p a

data Styled = Styled_
pattern Styled :: HasProp Styled a => Prop Styled a -> a -> a
pattern Styled p a <- (getProp Styled_ &&& id -> (p,a)) where
    Styled p a = setProp Styled_ p a

data Styles = Styles_
pattern Styles :: (HasProp Styles a, Prop Styles a ~ [(Txt,Txt)]) => [(Txt,Txt)] -> a -> a
pattern Styles p a <- (getProp Styles_ &&& id -> (p,a)) where
    Styles p a = setProp Styles_ (getProp Styles_ a ++ p) a

data Sub = Sub_
pattern Sub :: HasProp Sub a => Prop Sub a -> a -> a
pattern Sub p a <- (getProp Sub_ &&& id -> (p,a)) where
    Sub p a = setProp Sub_ p a

data Success = Success_
pattern Success :: HasProp Success a => Prop Success a -> a -> a
pattern Success p a <- (getProp Success_ &&& id -> (p,a)) where
    Success p a = setProp Success_ p a

data TabIndex = TabIndex_
pattern TabIndex :: HasProp TabIndex a => Prop TabIndex a -> a -> a
pattern TabIndex p a <- (getProp TabIndex_ &&& id -> (p,a)) where
    TabIndex p a = setProp TabIndex_ p a

data Tabular = Tabular_
pattern Tabular :: HasProp Tabular a => Prop Tabular a -> a -> a
pattern Tabular p a <- (getProp Tabular_ &&& id -> (p,a)) where
    Tabular p a = setProp Tabular_ p a

data Tag = Tag_
pattern Tag :: HasProp Tag a => Prop Tag a -> a -> a
pattern Tag p a <- (getProp Tag_ &&& id -> (p,a)) where
    Tag p a = setProp Tag_ p a

data Tertiary = Tertiary_
pattern Tertiary :: HasProp Tertiary a => Prop Tertiary a -> a -> a
pattern Tertiary p a <- (getProp Tertiary_ &&& id -> (p,a)) where
    Tertiary p a = setProp Tertiary_ p a

data Test = Test_
pattern Test :: HasProp Test a => Prop Test a -> a -> a
pattern Test p a <- (getProp Test_ &&& id -> (p,a)) where
    Test p a = setProp Test_ p a

data TextAlign = TextAlign_
pattern TextAlign :: HasProp TextAlign a => Prop TextAlign a -> a -> a
pattern TextAlign p a <- (getProp TextAlign_ &&& id -> (p,a)) where
    TextAlign p a = setProp TextAlign_ p a

data Threaded = Threaded_
pattern Threaded :: HasProp Threaded a => Prop Threaded a -> a -> a
pattern Threaded p a <- (getProp Threaded_ &&& id -> (p,a)) where
    Threaded p a = setProp Threaded_ p a

data Toggle = Toggle_
pattern Toggle :: HasProp Toggle a => Prop Toggle a -> a -> a
pattern Toggle p a <- (getProp Toggle_ &&& id -> (p,a)) where
    Toggle p a = setProp Toggle_ p a

data Total = Total_
pattern Total :: HasProp Total a => Prop Total a -> a -> a
pattern Total p a <- (getProp Total_ &&& id -> (p,a)) where
    Total p a = setProp Total_ p a

data TransitionOnMount = TransitionOnMount_
pattern TransitionOnMount :: HasProp TransitionOnMount a => Prop TransitionOnMount a -> a -> a
pattern TransitionOnMount p a <- (getProp TransitionOnMount_ &&& id -> (p,a)) where
    TransitionOnMount p a = setProp TransitionOnMount_ p a

data Transparent = Transparent_
pattern Transparent :: HasProp Transparent a => Prop Transparent a -> a -> a
pattern Transparent p a <- (getProp Transparent_ &&& id -> (p,a)) where
    Transparent p a = setProp Transparent_ p a

data Trigger = Trigger_
pattern Trigger :: HasProp Trigger a => Prop Trigger a -> a -> a
pattern Trigger p a <- (getProp Trigger_ &&& id -> (p,a)) where
    Trigger p a = setProp Trigger_ p a

data TriggerOn = TriggerOn_
pattern TriggerOn :: HasProp TriggerOn a => Prop TriggerOn a -> a -> a
pattern TriggerOn p a <- (getProp TriggerOn_ &&& id -> (p,a)) where
    TriggerOn p a = setProp TriggerOn_ p a

data Type = Type_
pattern Type :: HasProp Type a => Prop Type a -> a -> a
pattern Type p a <- (getProp Type_ &&& id -> (p,a)) where
    Type p a = setProp Type_ p a

data UI = UI_
pattern UI :: HasProp UI a => Prop UI a -> a -> a
pattern UI p a <- (getProp UI_ &&& id -> (p,a)) where
    UI p a = setProp UI_ p a

data Unit = Unit_
pattern Unit :: HasProp Unit a => Prop Unit a -> a -> a
pattern Unit p a <- (getProp Unit_ &&& id -> (p,a)) where
    Unit p a = setProp Unit_ p a

data UnmountOnHide = UnmountOnHide_
pattern UnmountOnHide :: HasProp UnmountOnHide a => Prop UnmountOnHide a -> a -> a
pattern UnmountOnHide p a <- (getProp UnmountOnHide_ &&& id -> (p,a)) where
    UnmountOnHide p a = setProp UnmountOnHide_ p a

data Unstackable = Unstackable_
pattern Unstackable :: HasProp Unstackable a => Prop Unstackable a -> a -> a
pattern Unstackable p a <- (getProp Unstackable_ &&& id -> (p,a)) where
    Unstackable p a = setProp Unstackable_ p a

data Upward = Upward_
pattern Upward :: HasProp Upward a => Prop Upward a -> a -> a
pattern Upward p a <- (getProp Upward_ &&& id -> (p,a)) where
    Upward p a = setProp Upward_ p a

data URL = URL_
pattern URL :: HasProp URL a => Prop URL a -> a -> a
pattern URL p a <- (getProp URL_ &&& id -> (p,a)) where
    URL p a = setProp URL_ p a

data Value = Value_
pattern Value :: HasProp Value a => Prop Value a -> a -> a
pattern Value p a <- (getProp Value_ &&& id -> (p,a)) where
    Value p a = setProp Value_ p a

data Vertical = Vertical_
pattern Vertical :: HasProp Vertical a => Prop Vertical a -> a -> a
pattern Vertical p a <- (getProp Vertical_ &&& id -> (p,a)) where
    Vertical p a = setProp Vertical_ p a

data VerticalAlign = VerticalAlign_
pattern VerticalAlign :: HasProp VerticalAlign a => Prop VerticalAlign a -> a -> a
pattern VerticalAlign p a <- (getProp VerticalAlign_ &&& id -> (p,a)) where
    VerticalAlign p a = setProp VerticalAlign_ p a

data Visible = Visible_
pattern Visible :: HasProp Visible a => Prop Visible a -> a -> a
pattern Visible p a <- (getProp Visible_ &&& id -> (p,a)) where
    Visible p a = setProp Visible_ p a

data Warning = Warning_
pattern Warning :: HasProp Warning a => Prop Warning a -> a -> a
pattern Warning p a <- (getProp Warning_ &&& id -> (p,a)) where
    Warning p a = setProp Warning_ p a

data Wide = Wide_
pattern Wide :: HasProp Wide a => Prop Wide a -> a -> a
pattern Wide p a <- (getProp Wide_ &&& id -> (p,a)) where
    Wide p a = setProp Wide_ p a

data Width = Width_
pattern Width :: HasProp Width a => Prop Width a -> a -> a
pattern Width p a <- (getProp Width_ &&& id -> (p,a)) where
    Width p a = setProp Width_ p a

data Widths = Widths_
pattern Widths :: HasProp Widths a => Prop Widths a -> a -> a
pattern Widths p a <- (getProp Widths_ &&& id -> (p,a)) where
    Widths p a = setProp Widths_ p a

data WithModal = WithModal_
pattern WithModal :: HasProp WithModal a => Prop WithModal a -> a -> a
pattern WithModal p a <- (getProp WithModal_ &&& id -> (p,a)) where
    WithModal p a = setProp WithModal_ p a

data WithPortal = WithPortal_
pattern WithPortal :: HasProp WithPortal a => Prop WithPortal a -> a -> a
pattern WithPortal p a <- (getProp WithPortal_ &&& id -> (p,a)) where
    WithPortal p a = setProp WithPortal_ p a

data WithRef = WithRef_
pattern WithRef :: HasProp WithRef a => Prop WithRef a -> a -> a
pattern WithRef p a <- (getProp WithRef_ &&& id -> (p,a)) where
    WithRef p a = setProp WithRef_ p a

data WithTransition = WithTransition_
pattern WithTransition :: HasProp WithTransition a => Prop WithTransition a -> a -> a
pattern WithTransition p a <- (getProp WithTransition_ &&& id -> (p,a)) where
    WithTransition p a = setProp WithTransition_ p a

data Wrapped = Wrapped_
pattern Wrapped :: HasProp Wrapped a => Prop Wrapped a -> a -> a
pattern Wrapped p a <- (getProp Wrapped_ &&& id -> (p,a)) where
    Wrapped p a = setProp Wrapped_ p a

pattern ToLeft = "left"
pattern ToRight = "right"
pattern ToTop = "top"
pattern ToBottom = "bottom"

pattern Middle = "middle"
pattern Bottom = "bottom"
pattern Top = "top"

pattern Mobile = "mobile"
pattern Tablet = "tablet"
pattern Computer = "computer"
pattern LargeScreen = "large screen"
pattern Widescreen = "widescreen"

pattern Red = "red"
pattern Orange = "orange"
pattern Yellow = "yellow"
pattern Olive = "olive"
pattern Green = "green"
pattern Teal = "teal"
pattern Blue = "blue"
pattern Violet = "violet"
pattern Purple = "purple"
pattern Pink = "pink"
pattern Brown = "brown"
pattern Grey = "grey"
pattern Gray = "grey"
pattern Black = "black"

pattern Mini c = Size "mini" c
pattern Tiny c = Size "tiny" c
pattern Small c = Size "small" c
pattern Medium c = Size "medium" c
pattern Large c = Size "large" c
pattern Big c = Size "big" c
pattern Huge c = Size "huge" c
pattern Massive c = Size "massive" c

pattern Unaligned c = TextAlign "" c
pattern LeftAligned c = TextAlign "left aligned" c
pattern RightAligned c = TextAlign "right aligned" c
pattern CenterAligned c = TextAlign "center aligned" c
pattern Justified c = TextAlign "justified" c

pattern Scale = "scale"
pattern Fade = "fade"
pattern FadeUp = "fade up"
pattern FadeDown = "fade down"
pattern FadeLeft = "fade left"
pattern FadeRight = "fade right"
pattern HorizontalFlip = "horizontal flip"
pattern VerticalFlip = "vertical flip"
pattern Drop = "drop"
pattern FlyLeft = "fly left"
pattern FlyRight = "fly right"
pattern FlyUp = "fly up"
pattern FlyDown = "fly down"
pattern SwingLeft = "swing left"
pattern SwingRight = "swing right"
pattern SwingUp = "swing up"
pattern SwingDown = "swing down"
pattern Browse = "browse"
pattern BrowseRight = "browse right"
pattern SlideDown = "slide down"
pattern SlideUp = "slide up"
pattern SlideRight = "slide right"

pattern Jiggle = "jiggle"
pattern Flash = "flash"
pattern Shake = "shake"
pattern Pulse = "pulse"
pattern Tada = "tada"
pattern Bounce = "bounce"

pattern MediumRectangle = "medium rectangle"
pattern LargeRectangle = "large rectangle"
pattern VerticalRectangle = "vertical rectangle"
pattern SmallRectangle = "small rectangle"
pattern MobileBanner = "mobile banner"
pattern Banner = "banner"
pattern VerticalBanner = "vertical banner"
pattern TopBanner = "top banner"
pattern HalfBanner = "half banner"
pattern ButonAd = "button"
pattern SquareButton = "square button"
pattern SmallButton = "small button"
pattern Skyscraper = "skyscraper"
pattern WideSkyscraper = "wide skyscraper"
pattern Leaderboard = "leaderboard"
pattern LargeLeaderboard = "large leaderboard"
pattern MobileLeaderboard = "mobile leaderboard"
pattern Billboard = "billboard"
pattern Panorama = "panorama"
pattern Netboard = "netboard"
pattern HalfPage = "half page"
pattern Square = "square"
pattern SmallSquare = "small square"

pattern Equal = "equal"
pattern One = "one"
pattern Two = "two"
pattern Three = "three"
pattern Four = "four"
pattern Five = "five"
pattern Six = "six"
pattern Seven = "seven"
pattern Eight = "eight"
pattern Nine = "nine"
pattern Ten = "ten"
pattern Eleven = "eleven"
pattern Twelve = "twelve"
pattern Thirteen = "thirteen"
pattern Fourteen = "fourteen"
pattern Fifteen = "fifteen"
pattern Sixteen = "sixteen"


module Semantic.Properties (module Semantic.Properties, module Export) where

import Semantic.Properties.Action as Export
import Semantic.Properties.Active as Export
import Semantic.Properties.Animated as Export
import Semantic.Properties.As as Export
import Semantic.Properties.Attached as Export
import Semantic.Properties.Attributes as Export
import Semantic.Properties.Avatar as Export
import Semantic.Properties.Basic as Export
import Semantic.Properties.Block as Export
import Semantic.Properties.Bordered as Export
import Semantic.Properties.Borderless as Export
import Semantic.Properties.Bulleted as Export
import Semantic.Properties.Celled as Export
import Semantic.Properties.Centered as Export
import Semantic.Properties.Children as Export
import Semantic.Properties.Circular as Export
import Semantic.Properties.Classes as Export
import Semantic.Properties.Clearing as Export
import Semantic.Properties.Close as Export
import Semantic.Properties.Collapsing as Export
import Semantic.Properties.Color as Export
import Semantic.Properties.Columns as Export
import Semantic.Properties.Compact as Export
import Semantic.Properties.Completed as Export
import Semantic.Properties.Corner as Export
import Semantic.Properties.Definition as Export
import Semantic.Properties.Disabled as Export
import Semantic.Properties.Divided as Export
import Semantic.Properties.Dividing as Export
import Semantic.Properties.Doubling as Export
import Semantic.Properties.Empty as Export
import Semantic.Properties.Error as Export
import Semantic.Properties.Extra as Export
import Semantic.Properties.Fitted as Export
import Semantic.Properties.Fixed as Export
import Semantic.Properties.Flipped as Export
import Semantic.Properties.Floated as Export
import Semantic.Properties.Floating as Export
import Semantic.Properties.Fluid as Export
import Semantic.Properties.Focus as Export
import Semantic.Properties.Focused as Export
import Semantic.Properties.FullWidth as Export
import Semantic.Properties.Grouped as Export
import Semantic.Properties.Hidden as Export
import Semantic.Properties.Horizontal as Export
import Semantic.Properties.Indeterminate as Export
import Semantic.Properties.Index as Export
import Semantic.Properties.Info as Export
import Semantic.Properties.Inline as Export
import Semantic.Properties.Instant as Export
import Semantic.Properties.Internal as Export
import Semantic.Properties.Inverted as Export
import Semantic.Properties.IsContainer as Export
import Semantic.Properties.IsHeader as Export
import Semantic.Properties.IsIcon as Export
import Semantic.Properties.IsText as Export
import Semantic.Properties.ItemsPerRow as Export
import Semantic.Properties.Labeled as Export
import Semantic.Properties.LabelPosition as Export
import Semantic.Properties.Link as Export
import Semantic.Properties.Loading as Export
import Semantic.Properties.Localize as Export
import Semantic.Properties.Name as Export
import Semantic.Properties.Negative as Export
import Semantic.Properties.OnChange as Export
import Semantic.Properties.OnClick as Export
import Semantic.Properties.OnComputer as Export
import Semantic.Properties.OnDismiss as Export
import Semantic.Properties.OnLargeScreen as Export
import Semantic.Properties.Only as Export
import Semantic.Properties.OnMobile as Export
import Semantic.Properties.OnSubmit as Export
import Semantic.Properties.OnTablet as Export
import Semantic.Properties.OnWidescreen as Export
import Semantic.Properties.Ordered as Export
import Semantic.Properties.Padded as Export
import Semantic.Properties.Pagination as Export
import Semantic.Properties.Piled as Export
import Semantic.Properties.Pointing as Export
import Semantic.Properties.Position as Export
import Semantic.Properties.Positive as Export
import Semantic.Properties.Primary as Export
import Semantic.Properties.Raised as Export
import Semantic.Properties.Ref as Export
import Semantic.Properties.Relaxed as Export
import Semantic.Properties.Reply as Export
import Semantic.Properties.Required as Export
import Semantic.Properties.Reversed as Export
import Semantic.Properties.Ribbon as Export
import Semantic.Properties.Rotated as Export
import Semantic.Properties.Rounded as Export
import Semantic.Properties.Secondary as Export
import Semantic.Properties.Section as Export
import Semantic.Properties.Selectable as Export
import Semantic.Properties.Selection as Export
import Semantic.Properties.SingleLine as Export
import Semantic.Properties.Size as Export
import Semantic.Properties.Sortable as Export
import Semantic.Properties.Sorted as Export
import Semantic.Properties.Spaced as Export
import Semantic.Properties.Stackable as Export
import Semantic.Properties.Stacked as Export
import Semantic.Properties.Stretched as Export
import Semantic.Properties.Striped as Export
import Semantic.Properties.Structured as Export
import Semantic.Properties.Sub as Export
import Semantic.Properties.Success as Export
import Semantic.Properties.TabIndex as Export
import Semantic.Properties.Tabular as Export
import Semantic.Properties.Tag as Export
import Semantic.Properties.Tertiary as Export
import Semantic.Properties.Test as Export
import Semantic.Properties.TextAlign as Export
import Semantic.Properties.Toggle as Export
import Semantic.Properties.Transition as Export
import Semantic.Properties.Transparent as Export
import Semantic.Properties.Type as Export
import Semantic.Properties.UI as Export
import Semantic.Properties.Unit as Export
import Semantic.Properties.Unstackable as Export
import Semantic.Properties.Value as Export
import Semantic.Properties.Vertical as Export
import Semantic.Properties.VerticalAlign as Export
import Semantic.Properties.Visible as Export
import Semantic.Properties.Warning as Export
import Semantic.Properties.Width as Export
import Semantic.Properties.Widths as Export
import Semantic.Properties.Wrapped as Export

infixl 1 !%
(!%) c cs as = Children (cs (Attributes as c))

infixl 1 %!
(%!) c as cs = Attributes (as (Children cs c))

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
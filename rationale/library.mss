@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/library.mss,v $)
@comment($Revision: 1.3 $ $Date: 2006/02/07 07:47:51 $)

@LabeledSection{Predefined library}

@Subheading{Abstract}

@i{This paper
describes various improvements to the predefined library in Ada 2005.}

@i{There are a number of important new core packages in Ada 2005. These
include a number of packages for the manipulation of various types
of containers, packages for directory operations and packages providing
access to environment variables.}

@i{The entire ISO/IEC 10646:2003 character repertoire is now supported.
Program text may now include other alphabets (such as Cyrillic and
Greek) and wide-wide characters and strings are supported at run-time.
There are also some improvements to the existing character, string
and text input@en@;output packages.}

@i{The Numerics annex now includes vector and matrix operations including
those previously found in the secondary standard ISO/IEC 13813.}

@i{This is one of a number of papers concerning Ada 2005 which are being
published in the Ada User Journal. An earlier version of this paper
appeared in the Ada User Journal, Vol. 26, Number 4, December 2005.
Other papers in this series will be found in later issues of the Journal
or elsewhere on this website.}

@LabeledClause{Ada Issues: Predefined library}

@leading@;The WG9 guidance document
@LocalLink{Target=[R1],Sec=[References],Text={[1]}} says

"The main purpose of the Amendment is to address identified problems
in Ada that are interfering with Ada's usage or adoption, especially
in its major application areas (such as high-reliability, long-lived
real-time and/or embedded applications and very large complex systems).
The resulting changes may range from relatively minor, to more substantial."

Certainly one of the stated advantages of languages such as Java is
that they come with a huge predefined library. By contrast the Ada
library is somewhat Spartan and extensions to it should make Ada more
accessible.

The guidance document also warns about secondary standards. Its essence
is don't use secondary standards if you can get the material into
the RM itself. And please put the stuff on vectors and matrices from
ISO/IEC 13813 @LocalLink{Target=[R5],Sec=[References],Text={[5]}} into the RM.
The reason for this exhortation is that secondary standards have proved
themselves to be almost invisible and hence virtually useless.

@leading@;We have already discussed the additional library packages in the area
of tasking and real time in a previous paper (see @RefSecNum{Task termination},
@RefSecNum{Scheduling and dispatching}, and @RefSecNum{CPU clocks and timers}).
The following Ada issues
cover the relevant changes in other areas and are described in detail
in this paper:

@begin[Description]
@AILink{AI=[AI95-00161-01],Text=[161]}@\Preelaborable initialization

@AILink{AI=[AI95-00248-01],Text=[248]}@\Directory operations

@AILink{AI=[AI95-00270-01],Text=[270]}@\Stream item size control

@AILink{AI=[AI95-00273-01],Text=[273]}@\Use of PCS should not be normative

@AILink{AI=[AI95-00285-01],Text=[285]}@\Support for 16-bit and 32-bit characters

@AILink{AI=[AI95-00296-01],Text=[296]}@\Vector and matrix operations

@AILink{AI=[AI95-00301-01],Text=[301]}@\Operations on language-defined strings

@AILink{AI=[AI95-00302-04],Text=[302]}@\Container library

@AILink{AI=[AI95-00328-01],Text=[328]}@\Non-generic version of @exam[Complex_IO]

@AILink{AI=[AI95-00351-01],Text=[351]}@\Time operations

@AILink{AI=[AI95-00362-01],Text=[362]}@\Some predefined packages should be recategorized

@AILink{AI=[AI95-00366-01],Text=[366]}@\More liberal rules for @exam[Pure] units

@AILink{AI=[AI95-00370-01],Text=[370]}@\Add standard interface for environment variables

@AILink{AI=[AI95-00388-01],Text=[388]}@\Add Greek pi to @exam[Ada.Numerics]

@AILink{AI=[AI95-00395-01],Text=[395]}@\Clarifications concerning 16- and 32-bit characters

@AILink{AI=[AI95-00400-01],Text=[400]}@\Wide and wide-wide images

@AILink{AI=[AI95-00418-01],Text=[418]}@\Vector norm

@AILink{AI=[AI95-00427-01],Text=[427]}@\Default parameters and @exam[Calendar] operations

@AILink{AI=[AI95-00428-01],Text=[428]}@\Input@en@;output for bounded strings

@AILink{AI=[AI95-00441-01],Text=[441]}@\Null streams
@end[Description]

These changes can be grouped as follows.

First the container library is rather extensive and merits a whole paper alone
(@AILink{AI=[AI95-00302-03],Text=[302]}). We only refer to it here for
completeness.

New child packages of @exam[Calendar] provide extra facilities for
manipulating times and dates (@AILink{AI=[AI95-00351-01],Text=[351]},
@AILink{AI=[AI95-00427-01],Text=[427]}).

There are additional packages in the core library providing access
to aspects of the operational environment. These concern directory
operations (@AILink{AI=[AI95-00248-01],Text=[248]}) and
environment variables (@AILink{AI=[AI95-00370-01],Text=[370]}).

There are changes concerning characters both for writing program text
itself and for handling characters and strings at run time. There
is now support for 16- and 32-bit characters
(@AILink{AI=[AI95-00285-01],Text=[285]},
@AILink{AI=[AI95-00388-01],Text=[388]},
@AILink{AI=[AI95-00395-01],Text=[395]},
@AILink{AI=[AI95-00400-01],Text=[400]}), and there are additional
operations in the string packages (@AILink{AI=[AI95-00301-01],Text=[301]},
@AILink{AI=[AI95-00428-01],Text=[428]}).

The Numerics annex is enhanced by the addition of the vector and matrix
material previously in ISO/IEC 13813 plus some commonly required linear
algebra algorithms (@AILink{AI=[AI95-00296-01],Text=[296]},
@AILink{AI=[AI95-00418-01],Text=[418]}) and a trivial
addition concerning complex
input@en@;output (@AILink{AI=[AI95-00328-01],Text=[328]}).

The categorization of various predefined units has been changed in order to
remove unnecessary restrictions on their use in distributed systems and similar
applications (@AILink{AI=[AI95-00362-01],Text=[362]},
@AILink{AI=[AI95-00366-01],Text=[366]}). The new pragma
@exam[Preelaborable_Initialization] is introduced as well for similar reasons
(@AILink{AI=[AI95-00161-01],Text=[161]}). We can also group a minor change
to the Distributed Systems annex here
(@AILink{AI=[AI95-00273-01],Text=[273]}).

Finally there is new attribute @exam[Stream_Size] in order to increase the
portability of streams (@AILink{AI=[AI95-00270-01],Text=[270]}) and the
parameter @exam[Stream] of @exam[Read], @exam[Write] etc now has a null
exclusion (@AILink{AI=[AI95-00441-01],Text=[441]}).


@LabeledClause{The container library}


This is a huge addition to the language and is described in a separate
paper (see @RefSecNum{Containers}) for convenience.


@LabeledClause{Times and dates}


@leading@;The first change to note is that the subtype @exam[Year_Number] in
the package @exam[Ada.Calendar] in Ada 2005 is
@begin[Example]
@key[subtype] Year_Number @key[is] Integer @key[range] 1901 .. 2399;
@end[Example]

In Ada 95 (and in Ada 83) the range is 1901 .. 2099. This avoids the
leap year complexity caused by the 400 year rule at the expense of
the use of dates in the far future. But, the end of the 21st century
is perhaps not so far into the future, so it was decided that the
2.1k problem should be solved now rather than later. However, it was
decided not to change the lower bound because some systems are known
to have used that as a time datum. The upper bound was chosen in order
to avoid difficulties for implementations. For example, with one nanosecond
for @exam[Duration'Small], the type @exam[Time] can just be squeezed
into 64 bits.

Having grasped the nettle of doing leap years properly Ada 2005 dives
in and deals with leap seconds, time zones and other such matters
in pitiless detail.

There are three new child packages @exam[Calendar.Time_Zones],
@exam[Calendar.Arithmetic] and @exam[Calendar.Formatting]. We will look at
these in turn.

@leading@;The specification of the first is@Defn2{Term=[package],Sec=[Ada.Calendar.Time_Zones]}@Defn{Ada.Calendar.Time_Zones package}@Defn{Time_Zones package}

@begin[Example]
@key[package] Ada.Calendar.Time_Zones @key[is]

   -- @examcom[Time zone manipulation:]
   @key[type] Time_Offset@key[ is range] @en@;28*60 .. 28*60;
   Unknown_Zone_Error: @key[exception];

   @key[function] UTC_Time_Offset(Date: Time := Clock) @key[return] Time_Offset;
@key[end] Ada.Calendar.Time_Zones;
@end[Example]

Time zones are described in terms of the number of minutes different
from UTC (which curiously is short for Coordinated Universal Time);@Defn{UTC}@Defn{Coordinated Universal Time}
this is close to but not quite the same as Greenwich Mean Time (GMT)
and similarly does not suffer from leaping about in spring and falling
about in the autumn. It might have seemed more natural to use hours
but some places (for example India) have time zones which are not
an integral number of hours different from UTC.@Defn{time zones}

Time is an extraordinarily complex subject. The difference between
GMT and UTC is never more than one second but at the moment of writing
there is a difference of about 0.577 seconds. The BBC broadcast timesignals
based on UTC but call them GMT and with digital broadcasting they
turn up late anyway. The chronophile might find the website
@URLLink{URL=[http://www.merlyn.demon.co.uk/misctime.htm#GMT],
Text=[http://www.merlyn.demon.co.uk/misctime.htm#GMT]} of interest.

@leading@;So the function @exam[UTC_Time_Offset] applied in an Ada program in
Paris to a value of type @exam[Time] in summer should return a time
offset of 120 (one hour for European Central Time plus one hour for
daylight saving or heure d@rquote @latin1(233)t@latin1(233)). Remember
that the type
@exam[Calendar.Time] incorporates the date. To find the offset now
(that is, at the time of the function call) we simply write
@begin[Example]
Offset := UTC_Time_Offset;
@end[Example]

and then @exam[Clock] is called by default.

@leading@;To find what the offset was on Christmas Day 2000 we write
@begin[Example]
Offset := UTC_Time_Offset(Time_Of(2000, 12, 25));
@end[Example]

and this should return 60 in Paris. So the poor function has to remember
the whole history of local time changes since 1901 and predict them
forward to 2399 @en these Ada systems are pretty smart! In reality
the intent is to use whatever the underlying operating system provides.
If the information is not known then it can raise @exam[Unknown_Zone_Error].

Note that we are assuming that the package @exam[Calendar] is set
to the local civil (or wall clock) time. It doesn't have to be but
one expects that to be the normal situation. Of course it is possible
for an Ada system running in California to have @exam[Calendar] set
to the local time in New Zealand but that would be unusual. Equally,
@exam[Calendar] doesn't have to adjust with daylight saving but we
expect that it will. (No wonder that @exam[Ada.Real_Time] was introduced
for vital missions such as boiling an egg.)

@leading@keepnext@;A useful fact is that
@begin[Example]
Clock @en Duration(UTC_Time_Offset*60)
@end[Example]

gives UTC time @en provided we don't do this just as daylight saving
comes into effect in which case the call of @exam[Clock] and that
of @exam[UTC_Time_Offset] might not be compatible.

@leading@;More generally the type @exam[Time_Offset] can be used to represent
the difference between two time zones. If we want to work with the
difference between New York and Paris then we could say
@begin[Example]
NY_Paris: Time_Offset := @en@;360;
@end[Example]

The time offset between two different places can be greater than 24
hours for two reasons. One is that the International Date Line weaves
about somewhat and the other is that daylight saving time can extend
the difference as well. Differences of 26 hours can easily occur and
27 hours is possible. Accordingly the range of the type @exam[Time_Offset]
allows for a generous 28 hours.

@leading@;The package @exam[Calendar.Arithmetic] provides some awkward arithmetic
operations and also covers leap seconds. Its specification
is@Defn2{Term=[package],Sec=[Ada.Calendar.Arithmetic]}@Defn{Ada.Calendar.Arithmetic package}@Defn{Calendar.Arithmetic package}
@begin[Example]
@key[package] Ada.Calendar.Arithmetic @key[is]

   -- @examcom[Arithmetic on days:]
   @key[type] Day_Count @key[is range]
     @en@;366*(1+Year_Number'Last @en Year_Number'First)
     ..
     +366*(1+Year_Number'Last @en Year_Number'First);

   @key[subtype] Leap_Seconds_Count @key[is] Integer @key[range] @en@;2047 .. 2047;

   @key[procedure] Difference(
      Left, Right: @key[in] Time;
      Days: @key[out] Day_Count; Seconds: @key[out] Duration;
      Leap_Seconds: @key[out] Leap_Seconds_Count);

   @key[function] "+" (Left: Time; Right: Day_Count) @key[return] Time;
   @key[function] "+" (Left: Day_Count; Right: Time) @key[return] Time;
   @key[function] "@en" (Left: Time; Right: Day_Count) @key[return] Time;
   @key[function] "@en" (Left, Right: Time) @key[return] Day_Count;

@key[end] Ada.Calendar.Arithmetic;
@end[Example]

The range for @exam[Leap_Seconds_Count] is generous. It allows for
a leap second at least four times a year for the foreseeable future
@en the somewhat arbitrary range chosen allows the value to be accommodated
in 12 bits. And the 366 in @exam[Day_Count] is also a bit generous
@en but the true expression would be very unpleasant.

One of the problems with the old planet is that it is slowing down
and a day as measured by the Earth's rotation is now a bit longer
than 86,400 seconds. Naturally enough we have to keep the seconds
uniform and so in order to keep worldly clocks synchronized with the
natural day, an odd leap second has to be added from time to time.
This is always added at midnight UTC (which means it can pop up in
the middle of the day in other time zones). The existence of leap
seconds makes calculations with times somewhat tricky.@Defn{leap second}

The basic trouble is that we want to have our cake and eat it. We
want to have the invariant that a day has 86,400 seconds but unfortunately
this is not always the case.

The procedure @exam[Difference] operates on two values of type @exam[Time]
and gives the result in three parts, the number of days (an integer),
the number of seconds as a @exam[Duration] and the number of leap
seconds (an integer). If @exam[Left] is later then @exam[Right] then
all three numbers will be nonnegative; if earlier, then nonpositive.

Remember that @exam[Difference] like all these other operations always
works on local time as defined by the clock in @exam[Calendar] (unless
stated otherwise).

@leading@;Suppose we wanted to find the difference between noon on June 1st
1982 and 2pm on July 1st 1985 according to a system set to UTC. We
might write
@begin[Example]
Days: Day_Count;
Secs: Duration;
Leaps: Leap_Seconds_Count;
...
Difference(
      Time_Of(1985, 7, 1, 14*3600.0),
      Time_Of(1982, 6, 1, 12*3600.0), Days, Secs, Leaps);
@end[Example]

@leading@keepnext@;The results should be
@begin[Example]
Days = 365+366+365+30 = 1126,
Secs = 7200.0,
Leaps = 2.
@end[Example]

There were leap seconds on 30 June 1983 and 30 June 1985.

The functions @exam["+"] and @exam["@en"] apply to values of type
@exam[Time] and @exam[Day_Count] (whereas those in the parent @exam[Calendar]
apply only to @exam[Time] and @exam[Duration] and thus only work for
intervals of a day or so). Note that the function @exam["@en"] between
two values of type @exam[Time] in this child package produces the
same value for the number of days as the corresponding call of the
function @exam[Difference] @en leap seconds are completely ignored.
Leap seconds are in fact ignored in all the operations @exam["+"]
and @exam["@en"] in the child package.

@leading@;However, it should be noted that @exam[Calendar."@en"] counts the
true seconds and so the expression
@begin[Example]
Calendar."@en" (Time_Of(1985, 7, 1, 1*3600.0), Time_Of(1985, 6, 30, 23*3600.0))
@end[Example]

has the @exam[Duration] value @exam[7201.0] and not @exam[7200.0]
because of the leap second at midnight that night. (We are assuming
that our Ada system is running at UTC.) The same calculation in New
York will produce @exam[7200.0] because the leap second doesn't occur
until 4 am in EST (with daylight saving).

@leading@keepnext@;Note also that
@begin[Example]
Calendar."@en" (Time_Of(1985, 7, 1, 0.0), Time_Of(1985, 6, 30, 0.0))
@end[Example]

in Paris where the leap second occurs at 10pm returns @exam[86401.0]
whereas the same calculation in New York will return @exam[86400.0].

@leading@;The third child package @exam[Calendar.Formatting] has a variety of
functions. Its specification
is@Defn2{Term=[package],Sec=[Ada.Calendar.Formatting]}@Defn{Ada.Calendar.Formatting package}@Defn{Calendar.Formatting package}
@begin[Example]
@tabset{P49}
@key[with] Ada.Calendar.Time_Zones;
@key[use] Ada.Calendar.Time_Zones;
@key[package] Ada.Calendar.Formatting @key[is]

   -- @examcom[Day of the week:]
   @key[type] Day_Name @key[is] (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

   @key[function] Day_Of_Week(Date: Time) @key[return] Day_Name;

   -- @examcom[Hours:Minutes:Seconds access:]
   @key[subtype] Hour_Number @key[is] Natural @key[range] 0 .. 23;
   @key[subtype] Minute_Number @key[is] Natural @key[range] 0 .. 59;
   @key[subtype] Second_Number @key[is] Natural @key[range] 0 .. 59;
   @key[subtype] Second_Duration @key[is] Day_Duration @key[range] 0.0 .. 1.0;

   @key[function] Year(Date: Time; Time_Zone: Time_Offset := 0) @key[return] Year_Number;

   -- @examcom[similarly functions Month, Day, Hour, Minute]

   @key[function] Second(Date: Time) @key[return] Second_Number;

   @key[function] Sub_Second(Date: Time) @key[return] Second_Duration;

   @key[function] Seconds_Of(
      Hour: Hour_Number;
      Minute: Minute_Number;
      Second: Second_Number := 0;
      Sub_Second: Second_Duration := 0.0) @key[return] Day_Duration;

   @key[procedure] Split(
      Seconds: @key[in] Day_Duration;@\-- @examcom[(1)]
      Hour: @key[out] Hour_Number;
      Minute: @key[out] Minute_Number;
      Second: @key[out] Second_Number;
      Sub_Second: @key[out] Second_Duration);

   @key[procedure] Split(
      Date: @key[in] Time;@\-- @examcom[(2)]
      Year: @key[out] Year_Number;
      Month: @key[out] Month_Number;
      Day: @key[out] Day_Number;
      Hour: @key[out] Hour_Number;
      Minute: @key[out] Minute_Number;
      Second: @key[out] Second_Number;
      Sub_Second: @key[out] Second_Duration;
      Time_Zone: @key[in] Time_Offset := 0);

   @key[function] Time_Of(
      Year: Year_Number;
      Month: Month_Number;
      Day: Day_Number;
      Hour: Hour_Number;
      Minute: Minute_Number;
      Second: Second_Number;
      Sub_Second: Second_Duration := 0.0;
      Leap_Second: Boolean := False;
      Time_Zone: Time_Offset := 0) @key[return] Time;

   @key[function] Time_Of(
      Year: Year_Number;
      Month: Month_Number;
      Day: Day_Number;
      Seconds: Day_Duration;
      Leap_Second: Boolean := False;
      Time_Zone: Time_Offset := 0) @key[return] Time;

   @key[procedure] Split(
      Date: @key[in] Time;@\--@examcom[(3)]
      ... -- @examcom[as (2) but with additional parameter]
      Leap_Second: @key[out] Boolean;
      Time_Zone: @key[in] Time_Offset := 0);

   @key[procedure] Split(
      Date: @key[in] Time;@\-- @examcom[(4)]
      ... -- @examcom[as Calendar.Split]
      ... -- @examcom[but with additional parameters]
      Leap_Second: @key[out] Boolean;
      Time_Zone: @key[in] Time_Offset := 0);

   -- @examcom[Simple image and value:]
   @key[function] Image(
      Date: Time;
      Include_Time_Fraction: Boolean := False;
      Time_Zone: Time_Offset := 0) @key[return] String;

   @key[function] Value(Date: String; Time_Zone: Time_Offset := 0) @key[return] Time;

   @key[function] Image (
      Elapsed_Time: Duration;
      Include_Time_Fraction: Boolean := False) @key[return] String;

   @key[function] Value(Elapsed_Time: String) @key[return] Duration;

@key[end] Ada.Calendar.Formatting;

@end[Example]

The function @exam[Day_Of_Week] will be much appreciated. It is a
nasty calculation.

Then there are functions @exam[Year], @exam[Month], @exam[Day], @exam[Hour],
@exam[Minute], @exam[Second] and @exam[Sub_Second] which return the
corresponding parts of a @exam[Time] taking account of the time zone
given as necessary. It is unfortunate that functions returning the
parts of a time (as opposed to the parts of a date) were not provided
in @exam[Calendar] originally. All that @exam[Calendar] provides is
@exam[Seconds] which gives the number of seconds from midnight and
leaves users to chop it up for themselves. Note that @exam[Calendar.Second]
returns a @exam[Duration] whereas the function in this child package
is @exam[Seconds] which returns an @exam[Integer]. The fraction of
a second is returned by @exam[Sub_Second].

@leading@;Most of these functions have an optional parameter which is a time
zone offset. Wherever in the world we are running, if we want to know
the hour according to UTC then we write
@begin[Example]
Hour(Clock, UTC_Time_Offset)
@end[Example]

@leading@;If we are in New York and want to know the hour in Paris then we
write
@begin[Example]
Hour(Clock, @en@;360)
@end[Example]

since New York is 6 hours (360 minutes) behind Paris.

Note that @exam[Second] and @exam[Sub_Second] do not have the optional
@exam[Time_Offset] parameter because offsets are an integral number
of minutes and so the number of seconds does not depend upon the time
zone.

The package also generously provides four procedures @exam[Split]
and two procedures @exam[Time_Of]. These have the same general purpose
as those in @exam[Calendar]. There is also a function @exam[Seconds_Of].
We will consider them in the order of declaration in the package specification
above.

@leading@;The function @exam[Seconds_Of] creates a value of type
@exam[Duration] from components @exam[Hour], @exam[Minute], @exam[Second] and
@exam[Sub_Second]. Note that we can use this together with
@exam[Calendar.Time_Of] to create a value of type @exam[Time]. For example
@begin[Example]
T := Time_Of(2005, 4, 2, Seconds_Of(22, 4, 10, 0.5));
@end[Example]

makes the time of the instant when I (originally) typed that last
semicolon.

@leading@;The first procedure @exam[Split] is the reverse of @exam[Seconds_Of].
It decomposes a value of type @exam[Duration] into @exam[Hour], @exam[Minute],
@exam[Second] and @exam[Sub_Second]. It is useful with the function
@exam[Calendar.Split] thus
@begin[Example]
@tabset[P42]
Split(Some_Time, Y, M, D, Secs);@\-- @examcom[split time]
Split(Secs, H, M, S, SS);@\-- @examcom[split secs]
@end[Example]

The next procedure @exam[Split] (no 2) takes a @exam[Time] and a
@exam[Time_Offset] (optional) and decomposes the time into its seven
components. Note that the optional parameter is last for convenience. The
normal rule for parameters of predefined procedures is that parameters of mode
in are first and parameters of mode out are last. But this is a nuisance if
parameters of mode in have defaults since this forces named notation if the
default is used.

There are then two functions @exam[Time_Of] which compose a @exam[Time]
from its various constituents and the @exam[Time_Offset] (optional).
One takes seven components (with individual @exam[Hour], @exam[Minute]
etc) whereas the other takes just four components (with @exam[Seconds]
in the whole day). An interesting feature of these two functions is
that they also have a Boolean parameter @exam[Leap_Second] which by
default is @exam[False].

@leading@;The purpose of this parameter needs to be understood carefully.
Making up a typical time will have this parameter as @exam[False]. But suppose
we need to compose the time midway through the leap second that occurred
on 30 June 1985 and assign it to a variable @exam[Magic_Moment]. We
will assume that our @exam[Calendar] is in New York and set to EST
with daylight saving (and so midnight UTC is 8pm in New York). We
would write
@begin[Example]
Magic_Moment: Time := Time_Of(1985, 6, 30, 19, 59, 59, 0.5, True);
@end[Example]

@leading@;In a sense there were two 19:59:59 that day in New York. The proper
one and then the leap one; the parameter distinguishes them. So the
moment one second earlier is given by
@begin[Example]
Normal_Moment: Time := Time_Of(1985, 6, 30, 19, 59, 59, 0.5, False);
@end[Example]

We could have followed ISO and used 23:59:60 UTC and so have subtype
@exam[Second_Number] @key[is] @exam[Natural] @key[range] @exam[0 ..
60;] but this would have produced an incompatibility with Ada 95.

Note that if the parameter @exam[Leap_Second] is @exam[True] and the other
parameters do not identify a time of a leap second then @exam[Time_Error] is
raised.

@leading@;There are then two corresponding procedures @exam[Split] (nos 3 and
4) with an out parameter @exam[Leap_Second]. One produces seven components
and the other just four. The difference between this seven-component
procedure @exam[Split] (no 3) and the earlier @exam[Split] (no 2)
is that this one has the out parameter @exam[Leap_Second] whereas
the other does not. Writing
@begin[Example]
Split(Magic_Moment, 0, Y, M, D, H, M, S, SS, Leap);
@end[Example]

@leading@;results in @exam[Leap] being @exam[True] whereas
@begin[Example]
Split(Normal_Moment, 0, Y, M, D, H, M, S, SS, Leap);
@end[Example]

results in @exam[Leap] being @exam[False] but gives all the other
out parameters (@exam[Y], ... , @exam[SS]) exactly the same values.

@leading@;On the other hand calling the version of @exam[Split] (no 2) without
the parameter @exam[Leap_Second] thus
@begin[Example]
Split(Magic_Moment, 0, Y, M, D, H, M, S, SS);
Split(Normal_Moment, 0, Y, M, D, H, M, S, SS);
@end[Example]

produces exactly the same results for both calls.

The reader might wonder why there are two @exam[Splits] on @exam[Time]
with @exam[Leap_Second] but only one without. This is because the
parent package @exam[Calendar] already has the other one (although
without the time zone parameter). Another point is that in the case
of @exam[Time_Of], we can default the @exam[Leap] parameter being
of mode in but in the case of @exam[Split] the parameter has mode
out and cannot be omitted. It would be bad practice to encourage the
use of a dummy parameter which is ignored and hence there have to
be additional versions of @exam[Split].

@leading@;Finally, there are two pairs of functions @exam[Image] and
@exam[Value].
The first pair works with values of type @exam[Time]. A call of @exam[Image]
returns a date and time value in the standard ISO 8601 format. Thus
taking the @exam[Normal_Moment] above
@begin[Example]
Image(Normal_Moment)
@end[Example]

@leading@;returns the following string
@begin[Example]
@tabset[P35]
"1985-06-30 19:59:59"@\-- @examcom[in New York]
@end[Example]

@leading@;If we set the optional parameter @exam[Include_Time_Fraction] to
@exam[True] thus
@begin[Example]
Image(Normal_Moment, True)
@end[Example]

@leading@;then we get
@begin[Example]
"1985-06-30 19:59:59.50"
@end[Example]

@leading@;There is also the usual optional @exam[Time_Zone] parameter so we
could produce the time in Paris (from the program in New York) thus
@begin[Example]
Image(Normal_Moment, True, @en@;360)
@end[Example]

@leading@;giving
@begin[Example]
@tabset[P35]
"1985-07-01 02:59:59.50"@\-- @examcom[in Paris]
@end[Example]

The matching @exam[Value] function works in reverse as expected.

@leading@;We would expect to get exactly the same results with
@exam[Magic_Moment]. However, since some implementations might have an ISO
function available in their operating system it is also allowed to produce
@begin[Example]
@tabset[P35]
"1985-06-30 19:59:60"@\-- @examcom[in New York]
@end[Example]

@leading@;The other @exam[Image] and @exam[Value] pair work on values of type
@exam[Duration] thus
@begin[Example]
@tabset[P35]
Image(10_000.0)@\-- @examcom["02:46:40"]
@end[Example]

with the optional parameter @exam[Include_Time_Fraction] as before.
Again the corresponding function @exam[Value] works in reverse.


@LabeledClause{Operational environment}


Two new packages are added to Ada 2005 in order to aid communication
with the operational environment. They are @exam[Ada.Environment_Variables]
and @exam[Ada.Directories].

@leading@;The package @exam[Ada.Environment_Variables] has the following
specification@Defn2{Term=[package],Sec=[Ada.Environment_Variables]}@Defn{Ada.Environment_Variables package}@Defn{Environment_Variables package}
@begin[Example]
@key[package] Ada.Environment_Variables @key[is]
   @key[pragma] Preelaborate(Environment_Variables);

   @key[function] Value(Name: String) @key[return] String;
   @key[function] Exists(Name: String) @key[return] Boolean;
   @key[procedure] Set(Name: @key[in] String; Value: @key[in] String);

   @key[procedure] Clear(Name: @key[in] String);
   @key[procedure] Clear;

   @key[procedure] Iterate(Process: @key[not null access procedure] (Name, Value: @key[in] String));

@key[end] Ada.Environment_Variables;
@end[Example]

This package provides access to environment variables by name. What
this means and whether it is supported depends upon the implementation.
But most operating systems have environment variables of some sort.
And if not, the implementation is encouraged to simulate them.

The values of the variable are also implementation defined and so
simply represented by strings.

@leading@;The behaviour is straightforward. We might check to see if there is
a variable with the name @exam["Ada"] and then read and print her
value and set it to 2005 if it is not, thus
@begin[Example]
@tabset[P35]
@key[if not] Exists("Ada") @key[then]
   @key[raise] Horror;@\-- @examcom[quel dommage!]
@key[end if];

Put("Current value of Ada is ");  Put_Line(Value("Ada"));

@key[if] Value("Ada") /= "2005" @key[then]
   Put_Line("Revitalizing Ada now");
   Set("Ada", "2005");
@key[end if];
@end[Example]

The procedure @exam[Clear] with a parameter deletes the variable concerned.
Thus @exam[Clear("Ada")] eliminates her completely so that a subsequent
call @exam[Exists("Ada")] will return @exam[False]. Note that @exam[Set]
actually clears the variable concerned and then defines a new one
with the given name and value. The procedure @exam[Clear] without
a parameter clears all variables.

@leading@;We can iterate over the variables using the procedure @exam[Iterate].
For example we can print out the current state by
@begin[Example]
@key[procedure] Print_One(Name, Value: @key[in] String) @key[is]
@key[begin]
   Put_Line(Name & "=" & Value);
@key[end] Print_One;
...
Iterate(Print_One'Access);
@end[Example]

The procedure @exam[Print_One] prints the name and value of the variable
passed as parameters. We then pass an access to this procedure as
a parameter to the procedure @exam[Iterate] and @exam[Iterate] then
calls @exam[Print_One] for each variable in turn.

Note that the slave procedure has both @exam[Name] and @exam[Value]
as parameters. It might be thought that this was unnecessary since
the user can always call the function @exam[Value]. However, real
operating systems can sometimes have several variables with the same
name; providing two parameters ensures that the name/value pairs are
correctly matched.

Attempting to misuse the environment package such as reading a variable
that doesn't exist raises @exam[Constraint_Error] or @exam[Program_Error].

There are big dangers of race conditions because the environment variables
are really globally shared. Moreover, they might be shared with the
operating system itself as well as programs written in other languages.

A particular point is that we must not call the procedures @exam[Set]
or @exam[Clear] within a procedure passed as a parameter to @exam[Iterate].

@leading@;The other environment package is @exam[Ada.Directories]. Its specification
is@Defn2{Term=[package],Sec=[Ada.Directories]}@Defn{Ada.Directories package}@Defn{Directories package}
@begin[Example]
@key[with] Ada.IO_Exceptions;
@key[with] Ada.Calendar;
@key[package] Ada.Directories @key[is]

   -- @examcom[Directory and file operations:]
   @key[function] Current_Directory @key[return] String;
   @key[procedure] Set_Directory(Directory: @key[in] String);
   @key[procedure] Create_Directory(New_Directory: @key[in ]String; Form: @key[in ]String := "");
   @key[procedure] Delete_Directory(Directory: @key[in] String);
   @key[procedure] Create_Path(New_Directory: @key[in] String; Form: @key[in ]String := "");
   @key[procedure] Delete_Tree(Directory: @key[in] String);
   @key[procedure] Delete_File(Name: @key[in] String);
   @key[procedure] Rename(Old_Name: @key[in] String; New_Name: @key[in] String);
   @key[procedure] Copy_File(Source_Name: @key[in] String; Target_Name: @key[in] String; Form: @key[in] String := "");

   -- @examcom[File and directory name operations:]
   @key[function] Full_Name(Name: String) @key[return] String;
   @key[function] Simple_Name(Name: String) @key[return] String;
   @key[function] Containing_Directory(Name: String) @key[return] String;
   @key[function] Extension(Name: String) @key[return] String;
   @key[function] Base_Name(Name: String) @key[return] String;
   @key[function] Compose(Containing_Directory: String := ""; Name: String; Extension: String := "")
      @key[return] String;

   -- @examcom[File and directory queries:]
   @key[type] File_Kind @key[is] (Directory, Ordinary_File, Special_File);
   @key[type] File_Size @key[is range] 0 .. @examcom[implementation_defined];
   @key[function] Exists(Name: String) @key[return] Boolean;
   @key[function] Kind(Name: String) @key[return] File_Kind;
   @key[function] Size(Name: String) @key[return] File_Size;
   @key[function] Modification_Time(Name: String) @key[return] Ada.Calendar.Time;

   -- @examcom[Directory searching:]
   @key[type] Directory_Entry_Type @key[is limited private];
   @key[type] Filter_Type @key[is array ](File_Kind) @key[of] Boolean;
   @key[type] Search_Type @key[is limited private];
   @key[procedure] Start_Search(
      Search: @key[in out] Search_Type;
      Directory: @key[in] String; Pattern: @key[in] String;
      Filter: @key[in] Filter_Type := (@key[others] => True));
   @key[procedure] End_Search(Search: @key[in out] Search_Type);
   @key[function] More_Entries(Search: Search_Type) @key[return] Boolean;
   @key[procedure] Get_Next_Entry(
      Search: @key[in out] Search_Type;
      Directory_Entry: @key[out] Directory_Entry_Type);
   @key[procedure] Search(
      Directory: @key[in] String;
      Pattern: @key[in] String;
      Filter: @key[in] Filter_Type := (@key[others] => True);
      Process: @key[not null access procedure]
                 (Directory_Entry: @key[in] Directory_Entry_Type));

   -- @examcom[Operations on Directory Entries:]
   @key[function] Simple_Name(Directory_Entry: Directory_Entry_Type) @key[return] String;
   @key[function] Full_Name(Directory_Entry: Directory_Entry_Type) @key[return] String;
   @key[function] Kind(Directory_Entry: Directory_Entry_Type) @key[return] File_Kind;
   @key[function] Size(Directory_Entry: Directory_Entry_Type) @key[return] File_Size;
   @key[function] Modification_Time(Directory_Entry: Directory_Entry_Type)
      @key[return] Ada.Calendar.Time;

   Status_Error: @key[exception renames ]Ada.IO_Exceptions.Status_Error;
   Name_Error: @key[exception renames ]Ada.IO_Exceptions.Name_Error;
   Use_Error: @key[exception renames ]Ada.IO_Exceptions.Use_Error;
   Device_Error: @key[exception renames A]da.IO_Exceptions.Device_Error;
@key[private]
   -- @examcom[Not specified by the language]
@key[end] Ada.Directories;
@end[Example]

Most operating systems have some sort of tree-structured filing system.
The general idea of this package is that it allows the manipulation
of file and directory names as far as is possible in a unified manner
which is not too dependent on the implementation and operating system.

Files are classified as directories, special files and ordinary files.
Special files are things like devices on Windows and soft links on
Unix; these cannot be created or read by the predefined Ada input@en@;output
packages.

Files and directories are identified by strings in the usual way.
The interpretation is implementation defined.

@leading@;The full name of a file is a string such as
@begin[Example]
"c:\adastuff\rat\library.doc"
@end[Example]

@leading@;and the simple name is
@begin[Example]
"library.doc"
@end[Example]

@leading@;At least that is in good old DOS. In Windows XP it is something like
@begin[Example]
"C:\Documents and Settings\john.JBI3\My Documents\adastuff\rat\library.doc"
@end[Example]

@leading@;For the sake of illustration we will continue with the simple DOS
example. The current directory is that set by the @exam["cd"] command.
So assuming we have done
@begin[Example]
c:\>cd adastuff
c:\adastuff>
@end[Example]

@leading@;then the function @exam[Current_Directory] will return the string
@exam["c:\adastuff"]. The procedure @exam[Set_Directory] sets the
current default directory. The procedures @exam[Create_Directory]
and @exam[Delete_Directory] create and delete a single directory.
We can either give the full name or just the part starting from the
current default. Thus@Defn2{Term=[directory],Sec=[creation]}@Defn2{Term=[directory],Sec=[deletion]}@Defn{create directory}@Defn{delete directory}@Defn{current directory}
@begin[Example]
Create_Directory("c:\adastuff\history");
Delete_Directory("history");
@end[Example]

will cancel out.

@leading@;The procedure @exam[Create_Path] creates several nested directories
as necessary. Thus starting from the situation above, if we write
@begin[Example]
Create_Path("c:\adastuff\books\old");
@end[Example]

then it will first create a directory @exam["books"] in @exam["c:\adastuff"]
and then a directory @exam["old"] in @exam["books"]. On the other
hand if we wrote @exam[Create_Path("c:\adastuff\rat");] then it would
do nothing since the path already exists. The procedure @exam[Delete_Tree]
deletes a whole tree including subdirectories and files.

The procedures @exam[Delete_File], @exam[Rename] and @exam[Copy_File]
behave as expected. Note in particular that @exam[Copy_File] can be
used to copy any file that could be copied using a normal input@en@;output
package such as @exam[Text_IO]. For example, it is really tedious
to use @exam[Text_IO] to copy a file intact including all line and
page terminators. It is a trivial matter using @exam[Copy_File].

Note also that the procedures @exam[Create_Directory], @exam[Create_Path]
and @exam[Copy_File] have an optional @exam[Form] parameter. Like
similar parameters in the predefined input@en@;output packages the meaning
is implementation defined.

The next group of six functions, @exam[Full_Name], @exam[Simple_Name],
@exam[Containing_Directory], @exam[Extension], @exam[Base_Name] and
@exam[Compose] just manipulate strings representing file names and
do not in any way interact with the actual external file system. Moreover,
of these, only the behaviour of @exam[Full_Name] depends upon the
current directory.

@leading@;The function @exam[Full_Name] returns the full name of a file. Thus
assuming the current directory is still @exam["c:\adastuff"]@Defn2{Term=[full name],Sec=[of a file]}
@begin[Example]
Full_Name("rat\library.doc")
@end[Example]

@leading@;returns @exam["c:\adastuff\rat\library.doc"] and
@begin[Example]
Full_Name("library.doc")
@end[Example]

returns @exam["c:\adastuff\library.doc"]. The fact that such a file
does not exist is irrelevant. We might be making up the name so that
we can then create the file. If the string were malformed in some
way (such as @exam["66##77"]) so that the corresponding full name
if returned would be nonsense then @exam[Name_Error] is raised. But
@exam[Name_Error] is never raised just because the file does not exist.

@leading@;On the other hand
@begin[Example]
Simple_Name("c:\adastuff\rat\library.doc")
@end[Example]

@leading@;returns @exam["library.doc"] and not @exam["rat\library.doc"]. We
can also apply @exam[Simple_Name] to a string that does not go back
to the root. Thus@Defn2{Term=[simple name],Sec=[of a file]}@Defn{file name}
@begin[Example]
Simple_Name("rat\library.doc");
@end[Example]

is allowed and also returns @exam["library.doc"].

@leading@;The function @exam[Containing_Directory_Name] removes the simple name
part of the parameter. We can even write
@begin[Example]
Containing_Directory_Name("..\rat\library.doc")
@end[Example]

and this returns @exam["..\rat"]; note that it also removes the separator
@exam["\"].

@leading@;The functions @exam[Extension] and @exam[Base_Name] return the
corresponding parts of a file name thus@Defn2{Term=[base name],Sec=[of a file]}@Defn2{Term=[extension],Sec=[of a file]}
@begin[Example]
@tabset[P35]
Base_Name("rat\library.doc")@\-- @examcom["library"]
Extension("rat\library.doc")@\-- @examcom["doc"]
@end[Example]

Note that they can be applied to a simple name or to a full name or,
as here, to something midway between.

@leading@;The function @exam[Compose] can be used to put the various bits
together, thus
@begin[Example]
Compose("rat", "library", "doc")
@end[Example]

@leading@;returns @exam["rat\library.doc"]. The default parameters enable bits
to be omitted. In fact if the third parameter is omitted then the
second parameter is treated as a simple name rather than a base name.
So we could equally write
@begin[Example]
Compose("rat","library.doc")
@end[Example]

The next group of functions, @exam[Exists], @exam[Kind], @exam[Size]
and @exam[Modification_Time] act on a file name (that is the name
of a real external file) and return the obvious result. (The size
is measured in stream elements @en usually bytes.)@Defn2{Term=[size],Sec=[of a file]}@Defn{file size}
@Defn2{Term=[modification time],Sec=[of a file]}@Defn{file modification time}
@Defn2{Term=[kind],Sec=[of a file]}@Defn{file kind}
@Defn2{Term=[existence],Sec=[of a file]}@Defn{file existence}

Various types and subprograms are provided to support searching over@Defn{search directory}@Defn2{Term=[directory],Sec=[search]}
a directory structure for entities with appropriate properties. This
can be done in two ways, either as a loop under the direct control
of the programmer (sometimes called an active iterator) or via an
access to subprogram parameter (often called a passive iterator).
We will look at the active iterator approach first.@Defn{active iterator}@Defn{passive iterator}

@leading@;The procedures @exam[Start_Search], @exam[End_Search] and
@exam[Get_Next_Entry] and the function @exam[More_Entries] control the search
loop. The general pattern is
@begin[Example]
@tabset[P35]
Start_Search( ... );
@key[while] More_Entries( ... ) @key[loop]
   Get_Next_Entry( ... );
   ...@\-- @examcom[do something with the entry found]
@key[end loop];
End_Search( ... );
@end[Example]

Three types are involved. The type @exam[Directory_Entry_Type] is
limited private and acts as a sort of handle to the entries found.
Valid values of this type can only be created by a call of @exam[Get_Next_Entry]
whose second parameter is an out parameter of the type @exam[Directory_Entry_Type].
The type @exam[Search_Type] is also limited private and contains the
state of the search. The type @exam[Filter_Type] provides a simple
means of identifying the kinds of file to be found. It has three components
corresponding to the three values of the enumeration type @exam[File_Kind]
and is used by the procedure @exam[Start_Search].

@leading@;Suppose we want to look for all ordinary files with extension
@exam["doc"] in the directory @exam["c:\adastuff\rat"]. We could write
@begin[Example]
@tabset[P35]
Rat_Search: Search_Type;
Item: Directory_Entry_Type;
Filter: Filter_Type := (Ordinary_File => True, @key[others] => False);
...
Start_Search(Rat_Search, "c:\adastuff\rat", "*.doc", Filter);
@key[while] More_Entries(Rat_Search) @key[loop]
   Get_Next_Entry(Rat_Search, Item);
   ...@\-- @examcom[do something with Item]
@key[end loop];
End_Search(Rat_Search);
@end[Example]

The third parameter of @exam[Start_Search] (which is @exam["*.doc"]
in the above example) represents a pattern for matching names and
thus provides further filtering of the search. The interpretation
is implementation defined except that a null string means match everything.
However, we would expect that writing @exam["*.doc"] would mean search
only for files with the extension @exam["doc"].

@leading@;The alternative mechanism using a passive iterator is as follows.
We first declare a subprogram such as
@begin[Example]
@tabset[P35]
@key[procedure] Do_It(Item: @key[in] Directory_Entry_Type) @key[is]
@key[begin]
   ...@\-- @examcom[do something with item]
@key[end] Do_It;
@end[Example]

@leading@;and then declare a filter and call the procedure @exam[Search] thus
@begin[Example]
Filter: Filter_Type := (Ordinary_File => True, @key[others] => False);
...
Search("c:\adastuff\rat", "*.doc", Filter, Do_It'Access);
@end[Example]

The parameters of @exam[Search] are the same as those of @exam[Start_Search]
except that the first parameter of type @exam[Search_Type] is omitted
and a final parameter which identifies the procedure @exam[Do_It]
is added. The variable @exam[Item] which we declared in the active
iterator is now the parameter @exam[Item] of the procedure@exam[ Do_It].

Each approach has its advantages. The passive iterator has the merit
that we cannot make mistakes such as forget to call @exam[End_Search].
But some find the active iterator easier to understand and it can
be easier to use for parallel searches.

The final group of functions enables us to do useful things with the
results of our search. Thus @exam[Simple_Name] and @exam[Full_Name]
convert a value of @exam[Directory_Entry_Type] to the corresponding
simple or full file name. Having obtained the file name we can do
everything we want but for convenience the functions @exam[Kind],
@exam[Size] and @exam[Modification_Time ]are provided which also directly
take a parameter of @exam[Directory_Entry_Type].

@leading@;So to complete this example we might print out a table of the files
found giving their simple name, size and modification time. Using
the active approach the loop might then become
@begin[Example]
@key[while] More_Entries(Rat_Search) @key[loop]
   Get_Next_Entry(Rat_Search, Item);
   Put(Simple_Name(Item));  Set_Col(15);
   Put(Size(Item/1000));  Put(" KB");  Set_Col(25);
   Put_Line(Image(Modification_Time(Item)));
@key[end loop];
@end[Example]

@leading@;This might produce a table such as
@begin[Example]
@Tabset[P21, P35]
access.doc@\    152 KB@\2005-04-05  09:03:10
containers.doc@\    372 KB@\2005-06-14  21:39:05
general.doc@\    181 KB@\2005-03-03  08:43:15
intro.doc@\    173 KB@\2004-11-25  15:52:20
library.doc@\    149 KB@\2005-04-08  13:50:05
oop.doc@\    179 KB@\2005-02-25  18:34:55
structure.doc@\    151 KB@\2005-04-05  09:09:25
tasking.doc@\    174 KB@\2005-03-31  11:16:40
@end[Example]

Note that the function @exam[Image] is from the package
@exam[Ada.Calendar.Formatting] discussed in the previous section.

Observe that the search is carried out on the directory given and
does not look at subdirectories. If we want to do that then we can
use the function @exam[Kind] to identify subdirectories and then search
recursively.

It has to be emphasized that the package @exam[Ada.Directories] is
very implementation dependent and indeed might not be supported by
some implementations at all. Implementations are advised to provide
any additional useful functions concerning retrieving other information
about files (such as name of the owner or the original creation date)
in a child package @exam[Ada.Directories.Information].

Finally, note that misuse of the various operations will raise one
of the exceptions @exam[Status_Error], @exam[Name_Error], @exam[Use_Error]
or @exam[Device_Error] from the package @exam[IO_Exceptions].


@LabeledClause{Characters and strings}


An important improvement in Ada 2005 is the ability to deal with 16-
and 32-bit characters both in the program text and in the executing
program.

The fine detail of the changes to the program text are perhaps for
the language lawyer. The purpose is to permit the use of all relevant
characters of the entire ISO/IEC 10646:2003 repertoire. The most important
effect is that we can write programs using Cyrillic, Greek and other
character sets.

@leading@;A good example is provided by the addition of the constant
@begin[Example]
@Pi : @key[constant] := Pi;
@end[Example]

@leading@;to the package @exam[Ada.Numerics]. This enables us to write
mathematical programs in a more natural notation thus
@begin[Example]
Circumference: Float := 2.0 * @Pi * Radius;
@end[Example]

@leading@;Other examples might be for describing polar coordinates thus
@begin[Example]
R: Float := Sqrt(X*X + Y*Y);
@unicode(952): Angle := Arctan(Y, X);


@end[Example]

@leading@;and of course in France we can now declare a decent set of ingredients
for breakfast
@begin[Example]
@key[type] Breakfast_Stuff @key[is] (Croissant, Caf@latin1(233), @unicode(338)uf, Beurre);
@end[Example]

@leading@;Curiously, although the ligature @exam[@latin1(230)] is in Latin-1 and thus
available in Ada 95 in identifiers, the ligature @exam[@unicode(339)] is not
(for reasons we need not go into). However, in Ada 95, @exam[@unicode(339)]
is a character of the type @exam[Wide_Character] and so even in Ada
95 one can order breakfast thus
@begin[Example]
@tabset[P49]
Put("Deux @unicode(339)ufs easy-over avec jambon");@\ -- @examcom[wide string]
@end[Example]

@leading@;In order to manipulate 32-bit characters, Ada 2005 includes types
@exam[Wide_Wide_Character] and @exam[Wide_Wide_String] in the package
@exam[Standard] and the appropriate operations to manipulate them
in packages such as@Defn{Wide_Wide_Character}@Defn{Wide_Wide_String}
@begin[Example]
Ada.Strings.Wide_Wide_Bounded
Ada.Strings.Wide_Wide_Fixed@Defn2{Term=[package],Sec=[Ada.Strings.Wide_Wide_Fixed]}@Defn{Ada.Strings.Wide_Wide_Fixed package}@Defn{Strings.Wide_Wide_Fixed package}
Ada.Strings.Wide_Wide_Maps
Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants
Ada.Strings.Wide_Wide_Unbounded
Ada.Wide_Wide_Text_IO@Defn2{Term=[package],Sec=[Ada.Wide_Wide_Text_IO]}@Defn{Ada.Wide_Wide_Text_IO package}@Defn{Wide_Wide_Text_IO package}
Ada.Wide_Wide_Text_IO.Text_Streams
Ada.Wide_Wide_Text_IO.Complex_IO
Ada.Wide_Wide_Text_IO.Editing
@end[Example]

There are also new attributes @exam[Wide_Wide_Image], @exam[Wide_Wide_Value]
and @exam[Wide_Wide_Width] and so on.

The addition of wide-wide characters and strings introduces many additional
possibilities for conversions. Just adding these directly to the existing
package @exam[Ada.Characters.Handling] could cause ambiguities in
existing programs when using literals. So a new package @exam[Ada.Characters.
Conversions] has been added. This contains conversions in all combinations
between @exam[Character], @exam[Wide_Character] and @exam[Wide_Wide_Character]
and similarly for strings. The existing functions from @exam[Is_Character]
to @exam[To_Wide_String] in @exam[Ada.Characters.Handling] have been
banished to
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-14.html],Text=[Annex J]}.

The introduction of more complex writing systems makes the definition
of the case insensitivity of identifiers, (the equivalence between
upper and lower case), much more complicated.

@leading@;In some systems, such as the ideographic system used by Chinese,
Japanese and Korean, there is only one case, so things are easy. But in other
systems, like the Latin, Greek and Cyrillic alphabets, upper and lower
case characters have to be considered. Their equivalence is usually
straightforward but there are some interesting exceptions such as

@begin[Itemize]
Greek has two forms for lower case sigma (the normal
form @unicode(963) and the final form @unicode(962) which is used at the
end of a word). These both convert to the one upper case letter @unicode(931).

German has the lower case letter @latin1(223) whose upper case
form is made of two letters, namely SS.

Slovenian has a grapheme LJ which is considered a single
letter and has three forms: LJ, Lj and lj.
@end[Itemize]

The Greek situation used to apply in English where the long s was
used in the middle of words (where it looked like an f but without
a cross stroke) and the familiar short s only at the end. To modern
eyes this makes poetic lines such as "Where the bee sucks, there suck
I" somewhat dubious. (This is sung by Ariel in Act V Scene I of The
Tempest by William Shakespeare.)

The definition chosen for Ada 2005 closely follows those provided
by ISO/IEC 10646:2003 and by the Unicode Consortium; this hopefully
means that all users should find that the case insensitivity of identifiers
works as expected in their own language.

Of interest to all users whatever their language is the addition of
a few more subprograms in the string handling packages. As explained
in the Introduction, Ada 95 requires rather too many conversions between
bounded and unbounded strings and the raw type @exam[String] and,
moreover, multiple searching is inconvenient.

@leading@;The additional subprograms in the packages are as follows.

In the package @exam[Ada.Strings.Fixed] (assuming @key[use] @exam[Maps;]
for brevity)
@begin[Example]
@key[function] Index(
      Source: String; Pattern: String;
      From: Positive; Going: Direction := Forward;
      Mapping: Character_Mapping := Identity) @key[return] Natural;

@key[function] Index(
      Source: String; Pattern: String;
      From: Positive; Going: Direction := Forward;
      Mapping: Character_Mapping_Function) @key[return] Natural;

@key[function] Index(
      Source: String; Set: Character_Set;
      From: Positive; Test: Membership := Inside;
      Going: Direction := Forward) @key[return] Natural;

@key[function] Index_Non_Blank(
      Source: String;
      From: Positive; Going: Direction := Forward) @key[return] Natural;
@end[Example]

The difference between these and the existing functions is that these
have an additional parameter @exam[From]. This makes it much easier
to search for all the occurrences of some pattern in a string.

Similar functions are also added to the packages @exam[Ada.Strings.Bounded]
and @exam[Ada.Strings.Unbounded].

@leading@;Thus suppose we want to find all the occurrences of @exam["bar"] in
the string @exam["barbara barnes"] held in the variable @exam[BS]
of type @exam[Bounded_String]. (I have put my wife into lower case
for convenience.) There are 3 of course. The existing function @exam[Count]
can be used to determine this fact quite easily
@begin[Example]
@tabset[P35]
N := Count(BS, "bar")@\-- @examcom[is 3]
@end[Example]

@leading@;But we really need to know where they are; we want the corresponding
index values. The first is easy in Ada 95
@begin[Example]
@tabset[P35]
I := Index(BS, "bar")@\-- @examcom[is 1]
@end[Example]

@leading@;But to find the next one in Ada 95 we have to do something such as
take a slice by removing the first three characters and then search
again. This would destroy the original string so we need to make a
copy of at least part of it thus
@begin[Example]
@tabset[P35]
Part := Delete(BS, I, I+2);@\-- @examcom[2 is length "bar" @en 1]
I := Index(Part, "bar") + 3;@\-- @examcom[is 4]
@end[Example]

@leading@;and so on in the not-so-obvious loop. (There are other ways such as
making a complete copy first, this could either be in another bounded
string or perhaps it is simplest just to copy it into a normal @exam[String]
first; but whatever we do it is messy.) In Ada 2005, having found
the index of the first in @exam[I], we can find the second by writing
@begin[Example]
I := Index(BS, "bar", From => I+3);
@end[Example]

and so on. This is clearly much easier.

@leading@;The following are also added to @exam[Ada.Strings.Bounded]
@begin[Example]
@key[procedure] Set_Bounded_String(
      Target: @key[out] Bounded_String;
      Source: @key[in] String; Drop: @key[in] Truncation := Error);

@key[function] Bounded_Slice(
      Source: Bounded_String;
      Low: Positive; High: Natural) @key[return] Bounded_String;

@key[procedure] Bounded_Slice(
      Source: @key[in] Bounded_String;
      Target: @key[out] Bounded_String;
      Low: @key[in] Positive; High: @key[in] Natural);
@end[Example]

@leading@;The procedure @exam[Set_Bounded_String] is similar to the existing
function @exam[To_Bounded_String]. Thus rather than
@begin[Example]
BS := To_Bounded_String("A Bounded String");
@end[Example]

@leading@;we can equally write
@begin[Example]
Set_Bounded_String(BS, "A Bounded String");
@end[Example]

@leading@;The slice subprograms avoid conversion to and from the type
@exam[String]. Thus to extract the characters from 3 to 9 we can write
@begin[Example]
@tabset[P35]
BS := Bounded_Slice(BS, 3, 9);@\-- @examcom["Bounded"]
@end[Example]

@leading@;whereas in Ada 95 we have to write something like

@begin[Example]
BS := To_Bounded(Slice(BS, 3, 9));
@end[Example]

Similar subprograms are added to @exam[Ada.Strings.Unbounded]. These
are even more valuable because unbounded strings are typically implemented
with controlled types and the use of a procedure such as @exam[Set_Unbounded_String]
is much more efficient than the function @exam[To_Unbounded_String]
because it avoids assignment and thus calls of @exam[Adjust].

@leading@;Input and output of bounded and unbounded strings in Ada 95 can only
be done by converting to or from the type @exam[String]. This is both
slow and untidy. This problem is particularly acute with unbounded
strings and so Ada 2005 provides the following additional package
(we have added a use clause for brevity as
usual)@Defn2{Term=[package],Sec=[Ada.Text_IO.Unbounded_IO]}@Defn{Ada.Text_IO.Unbounded_IO package}@Defn{Unbounded_IO package}
@begin[Example]
@key[with] Ada.Strings.Unbounded;  @key[use] Ada.Strings.Unbounded;
@key[package] Ada.Text_IO.Unbounded_IO @key[is]

   @key[procedure] Put(File: @key[in] File_Type; Item: @key[in] Unbounded_String);
   @key[procedure] Put(Item: @key[in] Unbounded_String);

   @key[procedure] Put_Line(File: @key[in] File_Type; Item: @key[in] Unbounded_String);
   @key[procedure] Put_Line(Item: @key[in] Unbounded_String);

   @key[function] Get_Line(File: File_Type) @key[return] Unbounded_String;
   @key[function] Get_Line @key[return ]Unbounded_String;

   @key[procedure] Get_Line(File: @key[in] File_Type; Item: @key[out] Unbounded_String);
   @key[procedure] Get_Line(Item: @key[out] Unbounded_String);

@key[end] Ada.Text_IO.Unbounded_IO;
@end[Example]

The behaviour is as expected.

There is a similar package for bounded strings but it is generic.
It has to be generic because the package @exam[Generic_Bounded_Length]
within @exam[Strings.Bounded] is itself generic and has to be instantiated
with the maximum string size. So the specification
is@Defn2{Term=[package],Sec=[Ada.Text_IO.Bounded_IO]}@Defn{Ada.Text_IO.Bounded_IO package}@Defn{Bounded_IO package}
@begin[Example]
@key[with] Ada.Strings.Bounded;  @key[use] Ada.Strings.Bounded;
@key[generic]
   @key[with package] Bounded @key[is new] Generic_Bounded_Length(<>);
   @key[use] Bounded;
@key[package] Ada.Text_IO.Bounded_IO @key[is]

   @key[procedure] Put(File: @key[in] File_Type; Item: @key[in] Bounded_String);
   @key[procedure] Put(Item: @key[in] Bounded_String);

... -- @examcom[etc as for Unbounded_IO]

@key[end] Ada.Text_IO.Bounded_IO;
@end[Example]

It will be noticed that these packages include functions @exam[Get_Line]
as well as procedures @exam[Put_Line] and @exam[Get_Line] corresponding
to those in @exam[Text_IO]. The reason is that procedures @exam[Get_Line]
are not entirely satisfactory.

If we do successive calls of the procedure @exam[Text_IO.Get_Line]
using a string of length 80 on a series of lines of length 80 (we
are reading a nice old deck of punched cards), then it does not work
as expected. Alternate calls return a line of characters and a null
string (the history of this behaviour goes back to early Ada 83 days
and is best left dormant).

@leading@;Ada 2005 accordingly adds corresponding functions @exam[Get_Line]
to the package @exam[Ada.Text_IO] itself thus
@begin[Example]
@key[function] Get_Line(File: File_Type) @key[return] String;
@key[function] Get_Line @key[return] String;
@end[Example]

Successive calls of a function @exam[Get_Line] then neatly return
the text on the cards one by one without bother.


@LabeledClause{Numerics annex}


When Ada 95 was being designed, the Numerics Rapporteur Group pontificated
at length over what features should be included in Ada 95 itself,
what should be placed in secondary standards, and what should be left
to the creativeness of the user community.

@leading@;A number of secondary standards had been developed for Ada 83. They
were
@begin[Description]
11430@\Generic package of elementary functions for Ada,

11729@\Generic package of primitive functions for Ada,

13813@\Generic package of real and complex type
declarations and basic operations for Ada (including vector and matrix types),

13814@\Generic package of complex elementary functions for Ada.
@end[Description]

The first two, 11430 and 11729, were incorporated into the Ada 95
core language. The elementary functions, 11430, (@exam[Sqrt], @exam[Sin],
@exam[Cos] etc) became the package @exam[Ada.Numerics.Generic_Elementary_
Functions] in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/A-5-1.html],Text=[A.5.1]},
and the primitive functions, 11729, became the
various attributes such as @exam[Floor], @exam[Ceiling], @exam[Exponent]
and @exam[Fraction] in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/A-5-3.html],Text=[A.5.3]}.
The original standards were withdrawn long ago.

The other two standards, although originally developed as Ada 83 standards
did not become finally approved until 1998.

In the case of 13814, the functionality was all incorporated into
the Numerics annex of Ada 95 as the package
@exam[Ada.Numerics.Generic_Complex_Elementary_Functions] in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/G-1-2.html],Text=[G.1.2]}.
Accordingly the original standard has now lapsed.

@leading@;However, the situation regarding 13813 was not so clear. It covered
four areas

@begin[Enumerate]
a complex types package including various complex arithmetic operations,

a real arrays package covering both vectors and matrices,

a complex arrays package covering both vectors and matrices, and

a complex input@en@;output package.
@end[Enumerate]

The first of these was incorporated into the Numerics annex of Ada
95 as the package @exam[Ada.Numerics.Generic_Complex_Types] in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/G-1-1.html],Text=[G.1.1]},
and the last similarly became the package @exam[Ada.Text_IO.Complex_IO] in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/G-1-3.html],Text=[G.1.3]}.
However, the array packages, both real and complex, were
not incorporated into Ada 95.

The reason for this omission is explained in
@URLLink{URL=[http://www.adaic.org/standards/95rat/RAThtml/rat95-p3-g.html#1],Text=[Section G.1.1]}
of the Rationale for Ada 95 @LocalLink{Target=[R3],Sec=[References],Text={[3]}} which says

@begin[SyntaxText]
A decision was made to abbreviate the Ada 95 packages by omitting
the vector and matrix types and operations. One reason was that such
types and operations were largely self-evident, so that little real
help would be provided by defining them in the language. Another reason
was that a future version of Ada might add enhancements for array
manipulation and so it would be inappropriate to lock in such operations
permanently.
@end[SyntaxText]

The sort of enhancements that perhaps were being anticipated were
facilities for manipulating arbitrary subpartitions of arrays such
as were provided in Algol 68. These rather specialized facilities
have not been added to Ada 2005 and indeed it seems most unlikely
that they would ever be added. The second justification for omitting
the vector and matrix facilities of 13813 thus disappears.

In order to overcome the objection that everything is self-evident
we have taken the approach that we should further add some basic facilities
that are commonly required, not completely trivial to implement, but
on the other hand are mathematically well understood.

@leading@;So the outcome is that Ada 2005 includes almost everything from 13813
plus subprograms for

@begin[Itemize]
finding the norm of a vector,@Defn{vector norm}@Defn2{Term=[norm],Sec=[vector]}

solving sets of linear equations,@Defn2{Term=[linear equations],Sec=[solving]}

finding the inverse and determinant of a matrix,@Defn2{Term=[inverse],Sec=[matrix]}@Defn{determinant}

finding the eigenvalues and eigenvectors of a symmetric
real or Hermitian matrix.@Defn{eigenvalues}@Defn{eigenvectors}
@end[Itemize]

A small number of operations that were not related to linear algebra
were removed (such as raising all elements of a matrix to a given
power).

@leading@;So Ada 2005 includes two new packages which are @exam[Ada.Numerics.Generic_Real_Arrays]
and @exam[Ada.Numerics.Generic_Complex_Arrays]. It would take too
much space to give the specifications of both in full so we give just
an abbreviated form of the real package in which the specifications
of the usual operators are omitted
thus@Defn2{Term=[package],Sec=[Ada.Numerics.Generic_Real_Arrays]}@Defn{Ada.Numerics.Generic_Real_Arrays package}@Defn{Generic_Real_Arrays package}
@begin[Example]
@key[generic]
   @key[type] Real@key[ is digits] <>;
@key[package] Ada.Numerics.Generic_Real_Arrays @key[is]
   @key[pragma] Pure(Generic_Real_Arrays);

   -- @examcom[Types]
   @key[type] Real_Vector @key[is array] (Integer @key[range] <>) @key[of] Real'Base;
   @key[type] Real_Matrix @key[is array] (Integer @key[range] <>, Integer @key[range] <>) @key[of] Real'Base;

   -- @examcom[Real_Vector arithmetic operations]
   ... -- @examcom[unary and binary "+" and "@en" giving a vector]
   ... -- @examcom[also inner product and two versions of "abs" @en one returns a vector and the]
   ... -- @examcom[other a value of Real'Base]

   -- @examcom[Real_Vector scaling operations]
   ... -- @examcom[operations "*" and "/" to multiply a vector by a scalar and divide a vector by a scalar]

   -- @examcom[Other Real_Vector operations]
   @key[function] Unit_Vector(Index: Integer; Order: Positive; First: Integer := 1) @key[return] Real_Vector;

   -- @examcom[Real_Matrix arithmetic operations]
   ... -- @examcom[unary "+", "@en", "abs", binary "+", "@en" giving a matrix]
   ... -- @examcom["*" on two matrices giving a matrix, on a vector and a matrix giving a vector,]
   ... -- @examcom[outer product of two vectors giving a matrix, and of course]
   @key[function] Transpose(X: Real_Matrix) @key[return] Real_Matrix;

   -- @examcom[Real_Matrix scaling operations]
   ... -- @examcom[operations "*" and "/" to multiply a matrix by a scalar and divide a matrix by a scalar]

   -- @examcom[Real_Matrix inversion and related operations]
   @key[function] Solve(A: Real_Matrix; X: Real_Vector) @key[return] Real_Vector;
   @key[function] Solve(A, X: Real_Matrix) @key[return] Real_Matrix;
   @key[function] Inverse(A: Real_Matrix) @key[return] Real_Matrix;
   @key[function] Determinant(A: Real_Matrix) @key[return] Real'Base;

   -- @examcom[Eigenvalues and vectors of a real symmetric matrix]
   @key[function] Eigenvalues(A: Real_Matrix) @key[return] Real_Vector;
   @key[procedure] Eigensystem(
         A: @key[in] Real_Matrix;
         Values: @key[out] Real_Vector; Vectors: @key[out] Real_Matrix);

   -- @examcom[Other Real_Matrix operations]
   @key[function] Unit_Matrix(Order: Positive; First_1, First_2: Integer := 1) @key[return] Real_Matrix;

@key[end] Ada.Numerics.Generic_Real_Arrays;
@end[Example]

Many of these operations are quite self-evident. The general idea
as far as the usual arithmetic operations are concerned is that we
just write an expression in the normal way as illustrated in the Introduction.
But the following points should be noted.

@leading@;There are two operations @exam["]@key[abs]@exam["] applying to a
@exam[Real_Vector] thus
@begin[Example]
@key[function] "@key[abs]"(Right: Real_Vector) @key[return] Real_Vector;
@key[function] "@key[abs]"(Right: Real_Vector) @key[return] Real'Base;
@end[Example]

One returns a vector each of whose elements is the absolute value
of the corresponding element of the parameter (rather boring) and
the other returns a scalar which is the so-called L2-norm of the vector.
This is the square root of the inner product of the vector with itself
or @unicode[8730](@unicode(931)@i[x]@-{@i{i}}@i[x]@-{@i{i}}) @en or just
@unicode[8730](@i[x]@-{@i{i}}@i[x]@-{@i{i}}) using the summation convention
(which will be familiar to those who dabble in the relative world
of tensors). This is provided as a distinct operation in order to
avoid any intermediate overflow that might occur if the user were
to compute it directly using the inner product "*".

@leading@;There are two functions @exam[Solve] for solving one and several sets
of linear equations respectively.@Defn2{Term=[linear equations],Sec=[solving]}
Thus if we have the single set of @i[n] equations
@begin[Example]
@b{@i{Ax}} = @b{@i{y}}
@end[Example]

@leading@keepnext@;then we might write
@begin[Example]
X, Y: Real_Vector(1 .. N);
A: Real_Matrix(1 .. N, 1 .. N);
...
Y := Solve(A, X);
@end[Example]

@leading@;and if we have @i[m] sets of @i[n] equations we might write
@begin[Example]
XX, YY: Real_Matrix(1 .. N, 1 .. M)
A: Real_Matrix(1 .. N, 1 .. N);
...
YY := Solve(A, XX);
@end[Example]

@Leading@Defn{Determinant}@Defn2{Term=[Inverse],Sec=[of matrix]}The
functions @exam[Inverse] and
@exam[Determinant] are provided for
completeness although they should be used with care. Remember that
it is foolish to solve a set of equations by writing
@begin[Example]
Y := Inverse(A)*X;
@end[Example]

because it is both slow and prone to errors. The main problem with
@exam[Determinant] is that it is liable to overflow or underflow even
for moderate sized matrices. Thus if the elements are of the order
of a thousand and the matrix has order 10, then the magnitude of the
determinant will be of the order of 10@+{30}. The user may therefore
have to scale the data.

@Defn[eigenvalues]@Defn[eigenvectors]Two subprograms
are provided for determining the eigenvalues and eigenvectors
of a symmetric matrix. These are commonly required in many calculations
in domains such as elasticity, moments of inertia, confidence regions
and so on. The function @exam[Eigenvalues] returns the eigenvalues
(which will be non-negative) as a vector with them in decreasing order.
The procedure @exam[Eigensystem] computes both eigenvalues and vectors;
the parameter @exam[Values] is the same as that obtained by calling
the function @exam[Eigenvalues] and the parameter @exam[Vectors] is
a matrix whose columns are the corresponding eigenvectors in the same
order. The eigenvectors are mutually orthonormal (that is, of unit
length and mutually orthogonal) even when there are repeated eigenvalues.
These subprograms apply only to symmetric matrices and if the matrix
is not symmetric then @exam[Argument_Error] is raised.

Other errors such as the mismatch of array bounds raise @exam[Constraint_Error]
by analogy with built-in array operations.

@leading@;The reader will observe that the facilities provided here are rather
humble and presented in a simple black-box style. It is important
to appreciate that we do not see the Ada predefined numerics library
as being in any way in competition with or as a substitute for professional
libraries such as the renowned BLAS (Basic Linear Algebra Subprograms,
see www.netlib.org/blas). Indeed our overall goal is twofold

@begin[Itemize]
to provide commonly required simple facilities for the
user who is not a numerical professional,

to provide a baseline of types and operations that forms
a firm foundation for binding to more general facilities such as the BLAS.
@end[Itemize]

We do not expect users to apply the operations in our Ada packages
to the huge matrices that arise in areas such as partial differential
equations. Such matrices are often of a special nature such as banded
and need the facilities of a comprehensive numerical library. We have
instead striven to provide easy to use facilities for the programmer
who has a small number of equations to solve such as might arise in
navigational applications.

Simplicity is evident in that functions such as @exam[Solve] do not
reveal the almost inevitable underlying LU decomposition or provide
parameters controlling for example whether additional iterations should
be applied. However, implementations are advised to apply an additional
iteration and should document whether they do or not.

Considerations of simplicity also led to the decision not to provide
automatic scaling for the determinant or to provide functions for
just the largest eigenvalue and so on.

Similarly we only provide for the eigensystems of symmetric real matrices.
These are the ones that commonly arise and are well behaved. General
nonsymmetric matrices can be troublesome.

Appropriate accuracy requirements are specified for the inner product
and L2-norm operations. Accuracy requirements for @exam[Solve], @exam[Inverse],
@exam[Determinant], @exam[Eigenvalues] and @exam[Eigenvectors] are
implementation defined which means that the implementation must document
them.

@leading@;The complex package is very similar and will not be described in
detail. However, the generic formal parameters are interesting. They are
@begin[Example]
@key[with] Ada.Numerics.Generic_Real_Arrays, Ada.Numerics.Generic_Complex_Types;
@key[generic]
   @key[with] @key[package] Real_Arrays @key[is] @key[new] Ada.Numerics.Generic_Real_Arrays(<>);
   @key[use] Real_Arrays;
   @key[with] @key[package] Complex_Types @key[is] @key[new] Ada.Numerics.Generic_Complex_Types(Real);
   @key[use] Complex_Types;
@key[package] Ada.Numerics.Generic_Complex_Arrays @key[is]
   ...
@end[Example]

@Defn2{Term=[package],Sec=[Ada.Numerics.Generic_Complex_Arrays]}@Defn{Ada.Numerics.Generic_Complex_Arrays package}@Defn{Generic_Complex_Arrays package}Thus
we see that it has two formal packages which are the corresponding
real array package and the existing Ada 95 complex types and operations
package. The formal parameter of the first is @exam[<>] and that of
the second is @exam[Real] which is exported from the first package
and ensures that both are instantiated with the same floating point
type.

As well as the obvious array and matrix operations, the complex package
also has operations for composing complex arrays from cartesian and
polar real arrays, and computing the conjugate array by analogy with
scalar operations in the complex types package. There are also mixed
real and complex array operations but not mixed imaginary, real and
complex array operations. Altogether the complex array package declares
some 80 subprograms (there are around 30 in the real array package)
and adding imaginary array operations would have made the package
unwieldy (and the reference manual too heavy).

By analogy with real symmetric matrices, the complex package has subprograms
for determining the eigensystems of Hermitian matrices. A Hermitian
matrix is one whose complex conjugate equals its transpose; such matrices
have real eigenvalues and are well behaved.@Defn{Hermitian matrix}
@Defn[eigenvalues]@Defn{eigenvectors}

We conclude this discussion of the Numerics annex by mentioning one
minute change regarding complex input@en@;output. Ada 2005 includes
preinstantiated forms of @exam[Ada.Text_IO.Complex_IO] such as @exam[Ada.Complex_Text_IO]
(for when the underlying real type is the type @exam[Float]),
@exam[Ada.Long_Complex_Text_IO] (for type @exam[Long_Float]) and so on.
These are by analogy with @exam[Float_Text_IO], @exam[Long_Float_Text_IO] and
their omission from Ada 95 was probably an oversight.


@LabeledClause{Categorization of library units}


@leading@;It will be recalled that library units in Ada 95 are categorized into
a hierarchy by a number of pragmas thus
@begin[Example]
@key[pragma] Pure( ... );
@key[pragma] Shared_Passive( ... );
@key[pragma] Remote_Types( ... );
@key[pragma] Remote_Call_Interface( ... );
@end[Example]

Each category imposes restrictions on what the unit can contain. An
important rule is that a unit can only depend on units in the same
or higher categories (the bodies of the last two are not restricted).

The pragmas @exam[Shared_Passive], @exam[Remote_Types], and
@exam[Remote_Call_Interface] concern distributed systems and thus are rather
specialized. A minor change made in the 2001 Corrigendum was that the pragma
@exam[Remote_Types] was added to the package @exam[Ada.Finalization] in order
to support the interchange of controlled types between partitions in a
distributed system.

@leading@;Note that the pragma @exam[Preelaborate] does not fit into this
hierarchy. In fact there is another hierarchy thus
@begin[Example]
@key[pragma] Pure( ... );
@key[pragma] Preelaborate( ... );
@end[Example]

and again we have the same rule that a unit can only depend upon units
in the same or higher category. Thus a pure unit can only depend upon
other pure units and a preelaborable unit can only depend upon other
preelaborable or pure units.

A consequence of this dual hierarchy is that a shared passive unit
cannot depend upon a preelaborable unit @en the units upon which it
depends have to be pure or shared passive and so on for the others.
However, there is a separate rule that a unit which is shared passive,
remote types or RCI must itself be preelaborable and so has to also
have the pragma @exam[Preelaborate].

The categorization of individual predefined units is intended to make
them as useful as possible. The stricter the category the more useful
the unit because it can be used in more circumstances.

The categorization was unnecessarily weak in Ada 95 in some cases
and some changes are made in Ada 2005.

@leading@;The following packages which had no categorization in Ada 95 have
pragma @exam[Preelaborate] in Ada 2005
@begin[Example]
Ada.Asynchronous_Task_Control
Ada.Dynamic_Priorities
Ada.Exceptions
Ada.Synchronous_Task_Control
Ada.Tags
Ada.Task_Identification
@end[Example]

@leading@;The following which had pragma @exam[Preelaborate] in Ada 1995 have
been promoted to pragma @exam[Pure] in Ada 2005
@begin[Example]
Ada.Characters.Handling
Ada.Strings.Maps
Ada.Strings.Maps.Constants
System
System.Storage_Elements
@end[Example]

These changes mean that certain facilities such as the ability to
analyse exceptions are now available to preelaborable units. Note
however, that @exam[Wide_Maps] and @exam[Wide_Maps.Wide_Constants]
stay as preelaborable because they may be implemented using access
types.

@leading@;Just for the record the following packages (and functions, @exam[Hash]
is a function) which are new to Ada 2005 have the pragma @exam[Pure]
@begin[Example]
Ada.Assertions
Ada.Characters.Conversions
Ada.Containers
Ada.Containers.Generic_Array_Sort
Ada.Containers.Generic_Constrained_Array_Sort
Ada.Dispatching
Ada.Numerics.Generic_Real_Arrays
Ada.Numerics.Generic_Complex_Arrays
Ada.Strings.Hash
@end[Example]

@leading@;And the following new packages and functions have the pragma @exam[Preelaborate]
@begin[Example]
Ada.Containers.Doubly_Linked_Lists
Ada.Containers.Hashed_Maps
Ada.Containers.Hashed_Sets
Ada.Containers.Ordered_Maps
Ada.Containers.Ordered_Sets
Ada.Containers.Vectors
Ada.Environment_Variables
Ada.Strings.Unbounded_Hash
Ada.Strings.Wide_Wide_Maps
Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants
Ada.Tags.Generic_Dispatching_Constructor
Ada.Task_Termination
@end[Example]

plus the indefinite containers as well.

@leading@;A problem with preelaborable units in Ada 95 is that there are
restrictions on declaring default initialized objects in a unit with the pragma
@exam[Preelaborate]. For example, we cannot declare objects of a private type
at the library level in such a unit. This is foolish for consider
@begin[Example]
@key[package] P @key[is]
   @key[pragma] Preelaborate(P);
   X: Integer := 7;
   B: Boolean := True;
@key[end];
@end[Example]

@leading@;Clearly these declarations can be preelaborated and so the package
@exam[P] can have the pragma @exam[Preelaborate]. However, now consider
@begin[Example]
@tabset[P35]
@key[package] Q @key[is]
   @key[pragma] Preelaborate(Q);@\-- @examcom[legal]
   @key[type] T @key[is private];
@key[private]
   @key[type] T @key[is]
      @key[record]
         X: Integer := 7;
         B: Boolean := True;
      @key[end record];
@key[end] Q;

@key[with] Q;
@key[package] P @key[is]
   @key[pragma] Preelaborate(P);@\-- @examcom[illegal]
   Obj: Q.T;
@key[end] P;
@end[Example]

The package @exam[Q] is preelaborable because it does not declare
any objects. However, the package @exam[P] is not preelaborable because
it declares an object of the private type @exam[T] @en the theory
being of course that since the type is private we do not know that
its default initial value is static.

@leading@Defn{Preelaborable_Initialization pragma}@Defn2{Term=[pragma],Sec=[Preelaborable_Initialization]}This
is overcome in Ada 2005 by the introduction of the pragma
@exam[Preelaborable_Initialization]. Its syntax is
@begin[Example]
@key[pragma] Preelaborable_Initialization(direct_name);
@end[Example]

@leading@keepnext@;We can now write
@begin[Example]
@key[package] Q @key[is]
   @key[pragma] Preelaborate(Q);
   @key[type] T @key[is private];
   @key[pragma] Preelaborable_Initialization(T);
@key[private]
   @key[type] T @key[is]
      @key[record]
         X: Integer := 7;
         B: Boolean := True;
      @key[end record];
@key[end] Q;
@end[Example]

The pragma promises that the full type will have preelaborable initialization
and the declaration of the package @exam[P] above is now legal.

@leading@;The following predefined private types which existed in Ada 95 have
the pragma @exam[Preelaborable_Initialization] in Ada 2005
@begin[Example]
Ada.Exceptions.Exception_Id
Ada.Exceptions.Exception_Occurrence
Ada.Finalization.Controlled
Ada.Finalization.Limited_Controlled
Ada.Numerics.Generic_Complex_Types.Imaginary
Ada.Streams.Root_Stream_Type
Ada.Strings.Maps.Character_Mapping
Ada.Strings.Maps.Character_Set
Ada.Strings.Unbounded.Unbounded_String
Ada.Tags.Tag
Ada.Task_Identification.Task_Id
Interfaces.C.Strings.chars_ptr
System.Address
System.Storage_Pool.Root_Storage_Pool
@end[Example]

Wide and wide-wide versions also have the pragma as appropriate. Note
that it was not possible to apply the pragma to
@exam[Ada.Strings.Bounded.Generic_Bounded_Length.Bounded_String]
because it would have made it impossible to instantiate
@exam[Generic_Bounded_Length]
with a non-static expression for the parameter @exam[Max].

@leading@;The following private types which are new in Ada 2005 also have the
pragma @exam[Preeleborable_Initialization]
@begin[Example]
Ada.Containers.Vectors.Vector
Ada.Containers.Vectors.Cursor
Ada.Containers.Doubly_Linked_Lists.List
Ada.Containers.Doubly_Linked_Lists.Cursor
Ada.Containers.Hashed_Maps.Map
Ada.Containers.Hashed_Maps.Cursor
Ada.Containers.Ordered_Maps.Map
Ada.Containers.Ordered_Maps.Cursor
Ada.Containers.Hashed_Sets.Set
Ada.Containers.Hashed_Sets.Cursor
Ada.Containers.Ordered_Sets.Set
Ada.Containers.Ordered_Sets.Cursor
@end[Example]

and similarly for the indefinite containers.

A related change concerns the definition of pure units. In Ada 2005,
pure units can now use access to subprogram and access to object types
provided that no storage pool is created.

Finally, we mention a small but important change regarding the partition
communication subsystem @exam[System.RPC]. Implementations conforming
to the Distributed Systems annex are not required to support this
predefined interface if another interface would be more appropriate
@en to interact with CORBA for example.


@LabeledClause{Streams}


Important improvements to the control of streams were described in
the paper on the object oriented model where we discussed the new
package @exam[Ada.Tags.Generic_Dispatching_Constructor] (see
@RefSecNum{Object factory functions}) and various changes to the parent
package @exam[Ada.Tags] itself. In this section we mention two other changes.

@leading@;There is a problem with the existing stream attributes such as (see
RM @URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-13-13-2.html],Text=[13.13.2]})
@begin[Example]
@key[procedure] S'Write(Stream: @key[access] Root_Stream_Type'Class; Item: @key[in] @examcom[T]);
@end[Example]
where @exam[S] is a subtype of @exam[T]. Note that for the parameter
@exam[Item], its type @exam[@i[T]] is in italic and so has
to be interpreted according to the kind of type. In the case of integer
and enumeration types it means that the parameter @exam[Item] has
type @exam[T'Base].

@leading@keepnext@;Given a declaration such as
@begin[Example]
@key[type] Index @key[is range] 1 .. 10;
@end[Example]

different implementations might use different representations for
@exam[Index'Base] @en some might use 8 bits others might use 32 bits
and so on.

Now stream elements themselves are typically 8 bits and so with an
8-bit base, there will be one value of @exam[Index] per stream element
whereas with a 32-bit base each value of @exam[Index] will take 4
stream elements. Clearly a stream written by the 8-bit implementation
cannot be read by the 32-bit one.

@leading@;This problem is overcome in Ada 2005 by the introduction of a new
attribute @exam[Stream_Size].@Defn{Stream_Size attribute}@Defn2{Term=[attribute],Sec=[Stream_Size]}
The universal integer value @exam[S'Stream_Size]
gives the number of bits used in the stream for values of the subtype
@exam[S]. We are guaranteed that it is a multiple of @exam[Stream_Element'Size].
So the number of stream elements required will be
@begin[Example]
S'Stream_Size / Stream_Element'Size
@end[Example]

@leading@;We can set the attribute in the usual way provided that the value
given is a static multiple of @exam[Stream_Element'Size]. So in the
case above we can write
@begin[Example]
@key[for] Index'Stream_Size @key[use] 8;
@end[Example]

and portability is then assured. That is provided that @exam[Stream_Element_Size]
is 8 anyway and that the implementation accepts the attribute definition
clause (which it should).

@leading@;A minor change is that the parameter @exam[Stream] of the various
atttributes now has a null exclusion so that @exam[S'Write] is in fact
@begin[Example]
@key[procedure] S'Write(Stream: @key[not null access] Root_Stream_Type'Class; Item: @key[in] @examcom[T]);
@end[Example]

Perhaps surprisingly this does not introduce any incompatibilities
since in Ada 95 passing null raises @exam[Constraint_Error] anyway
and so this change just clarifies the situation.

On this dullish but important topic here endeth the Rationale for
Ada 2005 apart from various exciting appendices and an extensive subpaper
on containers (see @RefSecNum{Containers}, or just press the Next button below).


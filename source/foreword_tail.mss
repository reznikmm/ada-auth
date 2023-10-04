@Part(foreword_tail, root="ada.mss")
@comment{$Source: e:\\cvsroot/ARM/Source/foreword_tail.mss,v $}
@comment{$Revision: 1.5 $ $Date: 2023/10/04 05:40:21 $}

@Comment{This file contains the ending shared part of the Foreword.}
@Comment{The beginning of the Foreword is found in ISO_Forward.mss and RM_Forward.mss}

@begin{Intro}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[Significant changes 
in this edition],ISOOnly=[The main changes]} are as follows:]}
@begin{Itemize}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[Improved],ISOOnly=[improved]}
support for parallel execution is provided via the introduction of parallel
loops, parallel blocks, parallel container iteration, and parallel
reduction@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[More],ISOOnly=[more]} precise
specification of subprogram interfaces is supported via the new aspects
Global, Global'Class, and Nonblocking. The Global aspects, in particular,
help to determine whether two constructs can
safely execute in parallel@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[Pre and Post aspects can now be specified for
access-to-subprogram types and for generic formal subprograms; a postcondition
for the default initialization of a type can be specified using the new
Default_Initial_Condition aspect@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[The],ISOOnly=[the]} behavior of
many predefined container operations is now more precisely specified by using
pre- and postcondition specifications instead of English descriptions; a
restricted (@lquotes@;stable@rquotes) view for most containers is introduced
to support more efficient iteration@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[More],ISOOnly=[more]} flexible
uses of static expressions are supported via the introduction of static
expression functions along with fewer restrictions on static
strings@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[The],ISOOnly=[the]} Image
attribute is supported for nonscalar types, and a user-specifiable attribute
Put_Image is provided, which determines the value of the Image attribute for
a user-defined type@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[The],ISOOnly=[the]} use of numeric
and string literals is generalized to allow their use with other categories of
types, via the new aspects Integer_Literal, Real_Literal, and
String_Literal@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[Array],ISOOnly=[array]} and
record aggregates are made more flexible: index parameters are allowed in an
array aggregate to define the components as a function of their array index;
discriminants can be defined more flexibly within an aggregate for a
variant record type@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[New],ISOOnly=[new]} types of
aggregates are provided: delta aggregates to allow the construction of a new
object by incremental updates to an existing object; container aggregates to
allow construction of an object of a container type by directly specifying
its elements@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[A],ISOOnly=[a]} shorthand is
provided, using the token '@@', to refer to the target of an assignment
statement in the expression defining its new value@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[Declare],ISOOnly=[declare]}
expressions are provided that permit the definition and use of local constants
or renamings, to allow a large expression to be simplified by defining common
parts as named entities@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[Support],ISOOnly=[support]} for
lightweight iteration is added via the introduction of procedural
iterators@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[Support],ISOOnly=[support]} for the
map-reduce programming strategy is added via the introduction of reduction
expressions@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[For],ISOOnly=[for]} constructs
that use iterators of any sort, a filter can be specified that restricts the
elements produced by the iteration to those that satisfy the condition of the
filter@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[Predefined],ISOOnly=[predefined]}
packages supporting arbitrary-precision integer and real arithmetic
are provided@ISODiff{NotISO=[.],ISOOnly=[;]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[@ISODiff{NotISO=[The],ISOOnly=[the]} Jorvik profile
is introduced to support hard real-time applications that want to go beyond the
restrictions of the Ravenscar profile.]}

@end{Itemize}

@begin{ISOOnly}@Comment{Rest of ISO boilerplate}

Any feedback or questions on this document should be directed to the user@rquote@;s
national standards body. A complete listing of these bodies can be found at
@Urllink{URL=[https://www.iso.org/members.html],Text=[www.iso.org/members.html],AllFormats=[T]}
and @Urllink{URL=[https://www.iec.ch/national-committees],
Text=[www.iec.ch/national-committees],AllFormats=[T]}.
@end{ISOOnly}@Comment{End ISO boilerplate}

@end{Intro}

@Comment{There is not trailing content in the RM Foreword.}
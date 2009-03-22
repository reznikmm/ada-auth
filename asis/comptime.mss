@Part(comptime, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/comptime.mss,v $}
@comment{$Revision: 1.2 $ $Date: 2009/03/13 07:12:33 $}

@LabeledSection{package Asis.Compilation_Units.Times}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis.Compilation_Units],Child=[Times]}Asis.Compilation_Units.Times
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[with] Ada.Calendar;@*
@key[package] @ChildUnit{Parent=[Asis.Compilation_Units],Child=[Times]}Asis.Compilation_Units.Times @key[is]}]}

Asis.Compilation_Units.Times encapsulates the time related functions used
within ASIS.


@LabeledClause{type Time}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
ASIS uses the predefined Ada.Calendar.Time@Chg{Version=[2],New=[ and],Old=[.
ASIS uses the predefined]} Standard.Duration@Chg{Version=[2],New=[ for
time-related values],Old=[]}.
The constant Nil_ASIS_Time is defined to support time queries where a
time is unavailable/unknown.

@begin{DescribeCode}
@begin{Example}
@AdaObjDefn{Nil_ASIS_Time} : constant Ada.Calendar.Time :=
       Ada.Calendar.Time_Of (Year    => 1901,
                             Month   => 1,
                             Day     => 1,
                             Seconds => 0.0);
@end{Example}
@end{DescribeCode}


@LabeledClause{function Time_Of_Last_Update}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Time_Of_Last_Update} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
                             @key[return] Ada.Calendar.Time;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the unit to query.

Returns the time that this physical compilation unit was most recently
updated in its implementor's Ada Environment. This will often be the
time of its last compilation. The exact significance of the result is
implementation specific.
Returns Nil_ASIS_Time if the unit has a Nil or nonexistent unit kind, or if
the time of last update is not available, or not meaningful, for any
reason.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are appropriate]}.
@end{DescribeCode}


@LabeledClause{function Compilation_CPU_Duration}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_CPU_Duration} (Compilation_Unit : @key[in] Asis.Compilation_Unit)
                              @key[return] Standard.Duration;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the unit to query.

Returns the Central Processing Unit (CPU) duration used to compile the physical
compilation unit associated with the Compilation_Unit argument. The exact
significance, or accuracy, of the result is implementation specific. Returns a
duration of 0.0 if the unit has a Nil or nonexistent unit kind, or if
the CPU duration for the last compilation is not available for any reason.
Returns a duration of 86_400.0 if the CPU duration for the last compilation is
greater than 1 day.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are appropriate]}.
@end{DescribeCode}


@LabeledClause{function Attribute_Time }


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Attribute_Time}
            (Compilation_Unit : @key[in] Asis.Compilation_Unit;
             Attribute        : @key[in] Wide_String)
            @key[return] Ada.Calendar.Time;
@end{Example}

Compilation_Unit @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the unit to query.
Attribute @Chg{Version=[1],New=[specifies],Old=[       @en Specifies]} the name of the attribute to query.

Returns the Time value associated with the given attribute. Returns
Nil_ASIS_Time if the argument is a Nil_Compilation_Unit, the unit does
not have the given Attribute, or the implementation does not record times
for attributes.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Compilation_Unit expects any kind of unit],
Old=[All Unit_Kinds are appropriate]}.

Results of this query may vary across ASIS implementations.
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Compilation_Units.Times;]}
@end{Example}

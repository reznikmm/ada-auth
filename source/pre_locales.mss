@comment{ $Source: e:\\cvsroot/ARM/Source/pre_locales.mss,v $ }
@comment{ $Revision: 1.9 $ $Date: 2015/04/03 04:12:42 $ $Author: randy $ }
@Part(predefenviron, Root="ada.mss")

@Comment{$Date: 2015/04/03 04:12:42 $}

@LabeledAddedClause{Version=[3],Name=[The Package Locales]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[A @i{locale}@Defn{locale} identifies a geopolitical
place or region and its associated language, which can be used to determine
other internationalization-related characteristics.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library package
Locales has the following declaration:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{package} Ada.Locales @key{is}@ChildUnit{Parent=[Ada],Child=[Locales]}
   @key{pragma} Preelaborate(Locales);
   @key{pragma} Remote_Types(Locales);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0037-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Language_Code} @key[is @Chg{Version=[4],New=[new],Old=[array]}] @Chg{Version=[4],New=[String ],Old=[]}(1 .. 3)@Chg{Version=[4],New=[
      @key[with] Dynamic_Predicate =>
         (@key[for all] E @key[of] Language_Code => E @key[in]],Old=[ @key[of] Character @key[range]]} 'a' .. 'z'@Chg{Version=[4],New=[)],Old=[]};
   @key[type] @AdaTypeDefn{Country_Code} @key[is @Chg{Version=[4],New=[new],Old=[array]}] @Chg{Version=[4],New=[String ],Old=[]}(1 .. 2)@Chg{Version=[4],New=[
      @key[with] Dynamic_Predicate =>
         (@key[for all] E @key[of] Country_Code  => E @key[in]],Old=[ @key[of] Character @key[range]]} 'A' .. 'Z'@Chg{Version=[4],New=[)],Old=[]};]}

@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0037-1]}
  @ChgAdded{Version=[4],Text=[These types are derived from type
  String so that they can easily be converted to or from type String. That's
  important if one of these values needs to be input or displayed (via Text_IO,
  perhaps). We use the predicate to ensure that only possible component values
  are used. Ada does not allow converting between unrelated types with
  components that don't statically match, so we cannot declare new types with
  constrained components if we want conversions to or from type String.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{Language_Unknown} : @key[constant] Language_Code := "und";
   @AdaObjDefn{Country_Unknown} : @key[constant] Country_Code := "ZZ";]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Language} @key[return] Language_Code;
   @key[function] @AdaSubDefn{Country} @key[return] Country_Code;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Locales;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2],ARef=[AI05-0233-1]}
@ChgAdded{Version=[3],Text=[The @i{active locale}@Defn{active
locale}@Defn2{Term=[locale],Sec=[active]} is the locale associated with the
partition of the current task.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0233-1]}
  @ChgAdded{Version=[3],Text=[Some environments define both a system locale and
  the locale of the current user. For such environments, the active locale is
  that of current user if any; otherwise (as in a partition running on a server
  without a user), the system locale should be used.]}
@end{ImplNote}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2]}
@ChgAdded{Version=[3],Text=[Language_Code is a lower-case string representation
of an ISO 639-3 alpha-3 code that identifies a language.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Some common language codes are: "eng" @en English;
  "fra" @en French; "deu" @en German; "zho" @en Chinese. These are the same
  codes as used by POSIX systems. We considered including
  constants for the most common languages, but that was rejected as the
  likely source of continual arguments about the constant names and which
  languages are important enough to include.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2]}
@ChgAdded{Version=[3],Text=[Country_Code is an upper-case string representation
of an ISO 3166-1 alpha-2 code that identifies a country.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Some common country codes are: "CA" @en Canada;
  "FR" @en France; "DE" @en Germany; "IT" @en Italy; "ES" @en Spain;
  "GB" @en United Kingdom; "US" @en United States. These are the same codes
  as used by POSIX systems. We didn't include any
  country constants for the same reasons that we didn't include any language
  constants.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Function Language returns the code of the language
associated with the active locale. If the Language_Code associated with the
active locale cannot be determined from the environment, then Language returns
Language_Unknown.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Function Country returns the code of the country
associated with the active locale. If the Country_Code associated with the
active locale cannot be determined from the environment, then Country returns
Country_Unknown.]}

@end{StaticSem}

@begin{Extend2005}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0127-2],ARef=[AI05-0233-1]}
@ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
Package Locales is new.]}
@end{Extend2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0037-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Types Language_Code and Country_Code are defined with predicates rather than
  constrained components so that they can be converted to/from type String.
  This changes the exception raised from Constraint_Error to Assertion_Error
  if an assignment is attempted with an incorrect value. This could only
  matter if there is a handler specifically for Constraint_Error surrounding
  this assignment; as this exception raise is certainly caused by a bug
  (why would anyone want to use invalid language or country codes?), such a
  handler seems very unlikely. (In addition, this is a new Ada 2012 package,
  so there is not likely to be a lot of code using it.)]}
@end{Inconsistent2012}


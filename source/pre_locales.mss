@comment{ $Source: e:\\cvsroot/ARM/Source/pre_locales.mss,v $ }
@comment{ $Revision: 1.6 $ $Date: 2011/11/01 05:34:04 $ $Author: randy $ }
@Part(predefenviron, Root="ada.mss")

@Comment{$Date: 2011/11/01 05:34:04 $}

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
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Language_Code} @key[is array] (1 .. 3) @key[of] Character @key[range] 'a' .. 'z';
   @key[type] @AdaTypeDefn{Country_Code} @key[is array] (1 .. 2) @key[of] Character @key[range] 'A' .. 'Z';]}

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

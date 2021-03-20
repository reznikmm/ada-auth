@comment{ $Source: e:\\cvsroot/ARM/Source/pre_ada.mss,v $ }
@comment{ $Revision: 1.17 $ $Date: 00/03/08 Created by RLB to avoid Includes }
@Part(predefstandard, Root="ada.mss")

@Comment{$Date: 2021/03/18 10:02:18 $}

@LabeledClause{The Package Ada}

@begin{StaticSem}
@Leading@keepnext@;The following language-defined library package exists:
@begin{Example}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0414-1]}
@RootLibUnit{Ada}@key[package] Ada@Chg{Version=[5],New=[],Old=[ @key[is]]}
   @Chg{Version=[5],New=[@key[with]],Old=[ @key[pragma]]} Pure@Chg{Version=[5],New=[ @key[is]],Old=[(Ada);]}
@key[end] Ada;
@end{Example}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0414-1]}
Ada serves as the parent of most of the other language-defined library
units; its declaration is empty@Chg{Version=[5],New=[],Old=[ (except for the
@nt{pragma} Pure)]}.
@end{StaticSem}

@begin{Legality}
In the standard mode, it is illegal to compile a child of package
Ada.
@begin{Reason}
The intention is that mentioning, say, Ada.Text_IO in a
@nt{with_clause} is guaranteed (at least in the standard mode) to refer
to the standard version of Ada.Text_IO.
The user can compile a root library unit Text_IO that has no relation to
the standard version of Text_IO.
@end{Reason}
@begin{Ramification}
Note that Ada can have non-language-defined grandchildren,
assuming the implementation allows it.
Also, packages System and Interfaces can have children,
assuming the implementation allows it.
@end{Ramification}
@begin{ImplNote}
An implementation will typically support a nonstandard mode in
which compiling the language defined library units is allowed.
Whether or not this mode is made available to users is up to the
implementer.

An implementation could theoretically have private children of
Ada, since that would be semantically neutral.
However, a programmer cannot compile such a library unit.
@end{ImplNote}
@end{Legality}

@begin{Extend83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Defn{extensions to Ada 83}
This @Chg{Version=[3],New=[subclause],Old=[clause]} is new to Ada 95.
@end{Extend83}

@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/cannex.mss,v $)
@comment($Revision: 1.5 $ $Date: 2006/02/19 06:45:45 $)

@LabeledClause{Summary table}

This paper concludes with a table showing at a glance the
various facilities in the six main containers.

@leading@;In order to save space the following abbreviations are used in the
table:

@Comment{@begin[Example] --Original version:
@Tabset[P7,P35,P42]
T@\container type eg Map@\H_T@\Hash_Type
C: T@\Container: container type@\I_T@\Index_Type
P: C@\Position: Cursor@\K_T@\Key_Type
L, R@\Left, Right@\Ex_Index@\Extended_Index
C_T@\Count_Type@\B@\Boolean@last
E_T@\Element_Type
@end[Example]}
@table{Columns=[4],Alignment=[AllLeft],FirstColWidth=[1],
NoBreak=[T],Border=[T],SmallSize=[F],
Caption=[],
Headers=[Abbrv.@\Meaning@\Abbrv.@\Meaning],
Body=[@exam{T}@\@exam{container type eg Map}@\@exam{H_T}@\@exam{Hash_Type}
@exam{C: T}@\@exam{Container: container type}@\@exam{I_T}@\@exam{Index_Type}
@exam{P: C}@\@exam{Position: Cursor}@\@exam{K_T}@\@exam{Key_Type}
@exam{L, R}@\@exam{Left, Right}@\@exam{Ex_Index}@\@exam{Extended_Index}
@exam{C_T}@\@exam{Count_Type}@\@exam{B}@\@exam{Boolean}
@exam{E_T}@\@exam{Element_Type}@\@\]}

also Index @en means that another subprogram exists with similar parameters
except that the first parameters are of type Vector and Index_Type
(or Extended_Index) rather than those involving cursors.

also Key and also Element similarly apply to maps and sets respectively.

@table{Columns=[7],Alignment=[CenterExceptFirst],FirstColWidth=[4],
NoBreak=[F],Border=[T],SmallSize=[T],
Caption=[],
Headers=[@\@exam{vectors}@\@exam{lists}@\@exam{hashed maps}@\@exam{ordered maps}@\@exam{hashed sets}@\@exam{ordered sets}],
Body=[@exam{@key[generic]}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@ @ @key[type] Index_Type @key[is range] <>;}@\@exam{Y}@\@\@\@\@\
@exam{@ @ @key[type] Key_Type @key[is private];}@\@\@\@exam{Y}@\@exam{Y}@\@\
@exam{@ @ @key[type] Element_Type @key[is private];}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@ @ @key[with function] Hash( ... ) @key[return] Hash_Type;}@\@\@\@exam{on Key}@\@\@exam{on Element}@\
@exam{@ @ @key[with function] Equivalent_...(L, R: ...)@*@ @ @ @ @ @key[return] Boolean;}@\@\@\@exam{on Key}@\@\@exam{on Element}@\
@exam{@ @ @key[with function] "<" (L, R: ... )@*@ @ @ @ @ @key[return] Boolean is <>;}@\@\@\@\@exam{on Key}@\@\@exam{on Element}
@exam{@ @ @key[with function] "=" (L, R: E_T) @key[return] B is <>;}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[package] Ada.Containers.... @key[is]}@\@exam{Vectors}@\@exam{Doubly_@*Linked_@*Lists}@\@exam{Hashed_@*Maps}@\@exam{Ordered_@*Maps}@\@exam{Hashed_@*Sets}@\@exam{Ordered_@*Sets}
@exam{@key[pragma] Preelaborate( ... );}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\ @exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[function] Equivalent_...(L, R: ...) @key[return] Boolean;}@\@\@\@\@exam{on Key}@\@\@exam{on Element}
@exam{@key[subtype] Extended_Index ...@*No_Index: @key[constant] Ex_Ind := Ex_Ind'First;}@\@exam{Y}@\@\@\@\@\
@exam{@key[type] T @key[is tagged private];@*@key[pragma] Preelaborable_Initialization(T);}@\@exam{Vector}@\@exam{List}@\@exam{Map}@\@exam{Map}@\@exam{Set}@\@exam{Set}
@exam{@key[type] Cursor @key[is private];@*@key[pragma] Preelaborable_Initialization(Cursor);}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{Empty_T: @key[constant] T;}@\@exam{Vector}@\@exam{List}@\@exam{Map}@\@exam{Map}@\@exam{Set}@\@exam{Set}
@exam{No_Element: @key[constant] Cursor;}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\ @exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[function] "=" (Left, Right: T) @key[return] Boolean;}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[function] Equivalent_Sets(L, R: Set)@*@ @ @ @key[return] Boolean;@*@key[function] To_Set(New_Item: E_T) @key[return] Set;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[function] To_Vector(Length: C_T) @key[return] Vector;@*@key[function] To_Vector(@*@ @ @ New_Item: E_T;@*@ @ @ Length: C_T) @key[return] Vector;}@\@exam{Y}@\@\@\@\@\
@exam{@key[function] "&" (L, R: Vector) @key[return] Vector;@*@key[function] "&" (L: Vector; R: E_T) @key[return] Vector;@*@key[function] "&" (L: E_T; R: Vector) @key[return] Vector;@*@key[function] "&" (L, R: E_T) @key[return] Vector;}@\@exam{Y}@\@\@\@\@\
@exam{@key[function] Capacity(C: T) @key[return] C_T;@*@key[procedure] Reserve_Capacity(@*@ @ @ C: T; Capacity: C_T);}@\@exam{Y}@\@\@exam{Y}@\@\@exam{Y}@\
@exam{@key[function] Length(C: T) @key[return] Count_Type;}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\ @exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Set_Length(@*@ @ @ C: @key[in out] T; Length: @key[in] C_T);}@\ @exam{Y}@\@\@\@\@\
@exam{@key[function] Is_Empty(C: T) return B;@*@key[procedure] Clear(C: @key[in out] T);}@\@exam{Y}@\@exam{Y}@\ @exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[function] To_Cursor(C: Vector; Index: Ex_Ind)@* @ @ @ @ @ @ @key[return] Cursor;@*@key[function] To_Index(P: C) @key[return] Ex_Ind;}@\@exam{Y}@\@\@\@\@\
@exam{@key[function] Key(P: C) @key[return] K_T;}@\@\@\@exam{Y}@\@exam{Y}@\@\
@exam{@key[function] Element(P: C) @key[return] E_T;}@\@exam{Y@*also Index}@\@exam{Y}@\@exam{Y}@\ @exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Replace_Element(@*@ @ @ C: @key[in out] T; P: C;@*@ @ @ New_Item: E_T);}@\@exam{Y@*also Index}@\@exam{Y}@\@exam{Y}@\ @exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Query_Element(P: C;@*@ @ @ Process: @key[not null acc proc]( ... ) );}@\@exam{@key[in] Element@*also Index}@\@exam{@key[in] Element}@\@exam{@key[in] Key,@*@key[in] Element}@\@exam{@key[in] Key,@*@key[in] Element}@\@exam{@key[in] Element}@\@exam{@key[in] Element}
@exam{@key[procedure] Update_Element(C: @key[in out] T; P: C;@*@ @ @ Process: @key[not null acc proc]( ... ) );}@\@exam{@key[in out] Elem@*also Index}@\@exam{@key[in out] Elem}@\@exam{@key[in] Key,@*@key[in out] Elem}@\@exam{@key[in] Key,@*@key[in out] Elem}@\@\
@exam{@key[procedure] Move(Target, Source: @key[in out] T);}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Insert(@*@ @ @ C: @key[in out] Vector; Before: Ex_Ind;@*@ @ @ New_Item: Vector);@*@key[procedure] Insert(@*@ @ @ C: @key[in out] Vector; Before: Cursor;@*@ @ @ New_Item: Vector);@*@key[procedure] Insert(@*@ @ @ C: @key[in out] Vector; Before: Cursor;@*@ @ @ New_Item: Vector; Position: @key[out] Cursor);}@\@exam{Y}@\@\@\@\@\
@exam{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Before: C;@*@ @ @ New_Item: E_T; Count: C_T := 1);}@\@exam{Y@*also Index}@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Before: C;@*@ @ @ New_Item: E_T; Position: @key[out] Cursor;@*@ @ @ Count: C_T := 1);}@\@exam{Y}@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Before: C;@*@ @ @ Position: @key[out] Cursor; Count: C_T := 1);@*element has default value}@\@exam{Y@*also Index}@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Key: K_T;@*@ @ @ New_Item: E_T; Position: @key[out] Cursor;@*@ @ @ Inserted: @key[out] B);}@\@\@\@exam{Y}@\@exam{Y}@\@exam{Y (no key)}@\@exam{Y (no key)}
@exam{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Key: K_T;@*@ @ @ Position: @key[out] Cursor; Inserted: @key[out] B);@*element has default value}@\@\@\@exam{Y}@\@exam{Y}@\@\
@exam{@key[procedure] Insert(@*@ @ @ C: @key[in out] T; Key: K_T;@*@ @ @ New_Item: E_T);}@\@\@\@exam{Y}@\@exam{Y}@\@exam{Y (no key)}@\@exam{Y (no key)}
@exam{@key[procedure] Prepend(@*@ @ @ C: @key[in out] Vector;@*@ @ @ New_Item: Vector);}@\@exam{Y}@\@\@\@\@\
@exam{@key[procedure] Prepend(@*@ @ @ C: @key[in out] T;@*@ @ @ New_Item: E_T; Count: C_T := 1);}@\@exam{Y}@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Append(@*@ @ @ C: @key[in out] Vector;@*@ @ @ New_Item: Vector);}@\@exam{Y}@\@\@\@\@\
@exam{@key[procedure] Append(@*@ @ @ C: @key[in out] T;@*@ @ @ New_Item: E_T; Count: C_T := 1);}@\@exam{Y}@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Insert_Space(@*@ @ @ C: @key[in out] V; Before: Cursor;@*@ @ @ Position: @key[out] Cursor; Count: C_T := 1);}@\@exam{Y@*also Index}@\@\@\@\@\
@exam{@key[procedure] Include(@*@ @ @ C: @key[in out] T;@*@ @ @ Key: Key_Type; New_Item: E_T);}@\@\@\@exam{Y}@\@exam{Y}@\@exam{Y (no key)}@\@exam{Y (no key)}
@exam{@key[procedure] Replace(@*@ @ @ C: @key[in out] T;@*@ @ @ Key: Key_Type; New_Item: E_T);}@\@\@\@exam{Y}@\@exam{Y}@\@exam{Y (no key)}@\@exam{Y (no key)}
@exam{@key[procedure] Exclude(@*@ @ @ C: @key[in out] T;@*@ @ @ Key: Key_Type);}@\@\@\@exam{Y}@\@exam{Y}@\@exam{Y (Item not key)}@\@exam{Y (item not key)}
@exam{@key[procedure] Delete(@*@ @ @ C: @key[in out] T; P: @key[in out] C;@*@ @ @ Count: C_T := 1);}@\@exam{Y@*also Index}@\@exam{Y}@\@exam{Y (no count)@*also Key}@\@exam{Y (no count)@*also Key}@\@exam{Y (no count)@*also Element}@\@exam{Y (no count)@*also Element}
@exam{@key[procedure] Delete_First(@*@ @ @ C: @key[in out] T; Count: C_T := 1);@*@key[procedure] Delete_Last(@*@ @ @ C: @key[in out] T; Count: C_T := 1);}@\@exam{Y}@\@exam{Y}@\@\@exam{Y (no count)}@\@\@exam{Y (no count)}
@exam{@key[procedure] Reverse_Elements(C: @key[in out] T);}@\@exam{Y}@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Swap(C: @key[in out] T; I, J: Cursor);}@\@exam{Y@*also Index}@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Swap_Links(@*@ @ @ C: @key[in out] List;@*@ @ @ I, J: Cursor);}@\@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Splice(@*@ @ @ Target: @key[in out] List; Before: Cursor;@*@ @ @ Source: @key[in out] List);@*@key[procedure] Splice(@*@ @ @ Target: @key[in out] List; Before: Cursor;@*@ @ @ Source: @key[in out] List; Position: @key[in out] Cursor);@*@key[procedure] Splice(@*@ @ @ Container: @key[in out] List; Before: Cursor;@*@ @ @ Position: @key[in out] Cursor);}@\@\@exam{Y}@\@\@\@\
@exam{@key[procedure] Union(@*@ @ @ Target: @key[in out] Set;@*@ @ @ Source: Set);@*@key[function] Union(L, R: Set) @key[return] Set;@*@key[function] "or" (L, R: Set) @key[return] Set@*@ @ @ @key[renames] Union;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Intersection(@*@ @ @ Target: @key[in out] Set;@*@ @ @ Source: Set);@*@key[function] Intersection(L, R: Set) @key[return] Set;@*@key[function] "and" (L, R: Set) @key[return] Set@*@ @ @ @key[renames] Intersection;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Difference(@*@ @ @ Target: @key[in out] Set;@*@ @ @  Source: Set);@*@key[function] Difference(L, R: Set) @key[return] Set;@*@key[function] "@en" (L, R: Set) @key[return] Set@*@ @ @ @key[renames] Difference;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Symmetric_Difference(@*@ @ @ Target: @key[in out] Set;@*@ @ @ Source: Set);@*@key[function] Symmetric_Difference (L, R: Set)@*@ @ @ @key[return] Set;@*@key[function] "xor" (L, R: Set) @key[return] Set@*@ @ @ @key[renames] Symmetric_Difference;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[function] Overlap(L, R: Set) @key[return] Boolean;@*@key[function] Is_Subset(Subset: Set; Of_Set: Set)@*@ @ @ @key[return] B;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[function] First_Index(C: T) @key[return] Index_Type;}@\@exam{Y}@\@\@\@\@\
@exam{@key[function] First(C: T) @key[return] Cursor;}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[function] First_Element(C: T)@*@ @ @ @key[return] Element_Type;}@\@exam{Y}@\@exam{Y}@\@\@exam{Y}@\@\@exam{Y}
@exam{@key[function] First_Key(C: T) @key[return] Key_Type;}@\@\@\@\@exam{Y}@\@\
@exam{@key[function] Last_Index(C: T) @key[return] Ex_Ind;}@\@exam{Y}@\@\@\@\@\
@exam{@key[function] Last(C: T) @key[return] Cursor;}@\@exam{Y}@\@exam{Y}@\@\@exam{Y}@\@\@exam{Y}
@exam{@key[function] Last_Element(C: T)@*@ @ @ @key[return] Element_Type;}@\@exam{Y}@\@exam{Y}@\@\@exam{Y}@\@\@exam{Y}
@exam{@key[function] Last_Key(C: T) @key[return] Key_Type;}@\@\@\@\@exam{Y}@\@\
@exam{@key[function] Next(P: C) @key[return] Cursor;@*@key[procedure] Next(P: @key[in out] C);}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[function] Previous(P: C) @key[return] Cursor;@*@key[procedure] Previous(P: @key[in out] C);}@\@exam{Y}@\@exam{Y}@\@\@exam{Y}@\@\@exam{Y}
@exam{@key[function] Find_Index(@*@ @ @ C: T; Item: E_T;@*@ @ @ Index: I_T := I_T'First) @key[return] Ex_Ind;}@\@exam{Y}@\@\@\@\@\
@exam{@key[function] Find(C: T; ... ; P: C := No_Element)@*@ @ @ @key[return] Cursor;}@\@exam{Element}@\@exam{Element}@\@exam{Key (no position)}@\@exam{Key (no position)}@\@exam{Element (no position)}@\@exam{Element (no position)}
@exam{@key[function] Element(C: T; Key: K_T) @key[return] E_T;}@\@\@\@exam{Y}@\@exam{Y}@\@\
@exam{@key[function] Reverse_Find_Index(@*@ @ @ C: T; Item: E_T;@*@ @ @ Index: I_T := I_T'First) @key[return] Ex_Ind;}@\@exam{Y}@\@\@\@\@\
@exam{@key[function] Reverse_Find(@*@ @ @ C: T; ... ;@*@ @ @ P: C := No_Element) @key[return] Cursor;}@\@exam{Element}@\@exam{Element}@\@\@\@\
@exam{@key[function] Floor(C: T; ...) @key[return] Cursor;@*@key[function] Ceiling(C: T; ...) @key[return] Cursor;}@\@\@\@\@exam{Key: K_T}@\@\@exam{Item: E_T}
@exam{@key[function] Contains(C: T; ...) @key[return] Boolean;}@\@exam{Element}@\@exam{Element}@\@exam{Key}@\@exam{Key}@\@exam{Element}@\@exam{Element}
@exam{@key[function] Has_Element(P: C) @key[return] Boolean;}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[function] Equivalent_... (L, R: Cursor)@*@ @ @ @key[return] Boolean;@*@key[function] Equivalent_... (L: Cursor; R:...)@*@ @ @ @key[return] Boolean;@*@key[function] Equivalent_... (L:...; R: Cursor)@*@ @ @ @key[return] Boolean;}@\@\@\@exam{Keys}@\@\@exam{Elements}@\
@exam{@key[function] "<" (L, R: Cursor) @key[return] Boolean;@*@key[function] ">" (L, R: Cursor) @key[return] Boolean;@*@key[function] "<" (L, Cursor; R: ...) @key[return] Boolean;@*@key[function] ">" (L, Cursor; R: ...) @key[return] Boolean;@*@key[function] "<" (L:...; R: Cursor) @key[return] Boolean;@*@key[function] ">" (L:...; R: Cursor) @key[return] Boolean;}@\@\@\@\@exam{Key}@\@\@exam{Element}
@exam{@key[procedure] Iterate(C: @key[in] T;@*@ @ @ Process: @key[not null acc proc] (P: C) );}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Reverse_Iterate(C: @key[in] T;@*@ @ @ Process: @key[not null acc proc] (P: C) );}@\@exam{Y}@\@exam{Y}@\@\@exam{Y}@\@\@exam{Y}
@exam{@b[generic]@*@ @ @ @key[with] @key[function] "<" (Left, Right: E_T)@*@ @ @ @ @ @ @key[return] B is <>;@*@key[package] Generic_Sorting @key[is]@*@ @ @ @key[function] Is_Sorted(C: T) @key[return] Boolean;@*@ @ @ @key[procedure] Sort(C: @key[in out] T);@*@ @ @ @key[procedure] Merge(Target, Source: @key[in out] T);@*@key[end] Generic_Sorting;}@\@exam{Y}@\@exam{Y}@\@\@\@\
@exam{@b[generic]@*@ @ @ @key[type] Key_Type (<>) @key[is private];}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@ @ @key[with function] Key(Element: E_T)@*@ @ @ @ @ @ @key[return] Key_Type;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@ @ @key[with function] Hash(Key: K_T)@*@ @ @ @ @ @ @ @key[return] Hash_Type;}@\@\@\@\@\@exam{Y}@\
@exam{@ @ @key[with function] Equivalent_Keys (@*@ @ @ @ @ L, R: Key_Type) @key[return] Boolean;}@\@\@\@\@\@exam{Y}@\
@exam{@ @ @key[with function] "<" (L, R: Key_Type)@*@ @ @ @ @ @ @key[return] B is <>;}@\@\@\@\@\@\@exam{Y}
@exam{@key[package] Generic_Keys @key[is]}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[function] Equivalent_Keys(L, R: Key_Type)@*@ @ @ @key[return] B;}@\@\@\@\@\@\@exam{Y}
@exam{@key[function] Key(P: C) @key[return] Key_Type;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[function] Element(C: T; Key: K_T)@*@ @ @ @key[return] Element_T;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Replace(C: @key[in out] T; Key: Key_Type;@*@ @ @ New_Item: E_T);@*@key[procedure] Exclude(@*@ @ @ C: @key[in out] T;@*@ @ @ Key: Key_Type);@*@key[procedure] Delete(C: @key[in out] T; Key: Key_Type);}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[function] Find(C: T; Key: K_T) @key[return] Cursor;}@\@\@\@\@\@exam{Y}@\@exam{Y}
@exam{@key[function] Floor(C: T; Key: K_T) @key[return] Cursor;@*@key[function] Ceiling(C: T; Key: K_T) @key[return] Cursor;}@\ @\ @\ @\ @\ @\@exam{Y}
@exam{@key[function] Contains(C: T; Key: K_T)@*@ @ @ @key[return] Boolean;}@\ @\ @\ @\ @\@exam{Y}@\@exam{Y}
@exam{@key[procedure] Update_Element_Preserving_Key@*@ @ @ (C: @key[in out] T; P: C;@*@ @ @ Process: @key[not null acc proc] (@*@ @ @ @ @ @ Element: @key[in out] E_T) );}@\ @\ @\ @\ @\@exam{Y}@\@exam{Y}
@exam{@key[end] Generic_Keys;}@\@\@\@\@\@exam{Y}@\@exam{Y}@last
@exam{@key[private]@*  ... -- @i[not specified by the language]@*@key[end] Ada.Containers....;}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}@\@exam{Y}]}

@comment{ $Source: e:\\cvsroot/ARM/Source/pre_containers.mss,v $ }
@comment{ $Revision: 1.1 $ $Date: 2004/12/07 05:17:08 $ $Author: Randy $ }
@Part(precontainers, Root="ada.mss")

@Comment{$Date: 2004/12/07 05:17:08 $}

@LabeledAddedClause{Version=[2],Name=[Containers]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@Chg{Version=[2],New=[This clause presents the specifications of the package
Containers and several child packages, which provide facilities for storing
collections of elements.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@Chg{Version=[2],New=[A variety of sequence and associative containers are
provided. Each container includes a @i{cursor} type. A cursor is a reference
to an element within a container. Many operations on cursors are common to
all of the containers.@PDefn2{Term=[cursor],Sec=[for a container]}
@Defn2{Term=[container],Sec=[cursor]}],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@Chg{Version=[2],New=[Within this clause we provide Implementation Advice
for the desired average or worst case time complexity of certain operations
on a container. This advice is expressed using a big-O notation.
A complexity of O(f(N)), presuming f is some function of a
length parameter N and t(N) is the time the operation takes
(on average or worst case, as specified) for the length N,
means that there exists a finite A such that for any N, t(N)/f(N) < A.
@Defn{big-O notation}@Defn{O(f(N))}],Old=[]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @Chg{Version=[2],New=[Of course, an implementation can do better than a
  specified O(f(N)): for example, O(1) meets the requirements for O(log N).],Old=[]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[If the advice suggests that the complexity should be
less than O(f(N)), then for any arbitrarily small positive real D, there
should exist a positive integer M such that for all N > M,
t(N)/f(N) < D.],Old=[]}
@end{Intro}

@begin{Metarules}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@Chg{Version=[2],New=[This clause provides a number of useful containers
for Ada. Only the most useful containers are provided. Ones that are relatively
easy to code, redundant, or rarely used are omitted from this set, even if they
are generally included in containers libraries.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[The containers packages are modeled on the Standard Template Library (STL), an
algorithms and data structure library popularized by Alexander Stepanov, and
included in the C++ standard library. The structure and terminology differ from
the STL where that better maps to common Ada usage. For instance, what the STL
calls @lquotes@;iterators@rquotes@; are
called @lquotes@;cursors@rquotes@; here.],Old=[]}

**** The rest of this clause (including subclauses) has yet to be inserted ****

@end{Metarules}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}
This clause is new. It just provides an introduction.],Old=[]}
@end{Extend95}


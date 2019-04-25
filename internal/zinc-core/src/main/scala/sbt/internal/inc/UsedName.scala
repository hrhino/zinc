/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt.internal.inc

import java.{ lang => jl }

import scala.collection.JavaConverters._
import xsbti.UseScope

final case class UsedName(name: String, scopes: UsedName.UseScopeSet)

object UsedName {

  def apply(name: String, scopes: Iterable[UseScope] = Nil): UsedName = {
    val escapedName = escapeControlChars(name)
    UsedName(escapedName, UseScopeSet(scopes))
  }
  def apply(name: String, scopes: jl.Iterable[UseScope]): UsedName = {
    apply(name, scopes.asScala)
  }

  private def escapeControlChars(name: String) = {
    if (name.indexOf('\n') > 0) // optimize for common case to regex overhead
      name.replaceAllLiterally("\n", "\u26680A")
    else
      name
  }

  /** A set of [[UseScope]]s, inlined to save space. */
  final class UseScopeSet(val mask: Byte) extends AnyVal {
    import UseScopeSet._

    def contains(scope: UseScope): Boolean = (mask & toBit(scope)) != 0
    def toSet: Set[UseScope] = {
      var res = Set.empty[UseScope]
      if ((mask & DefaultScope) != 0) res += UseScope.Default
      if ((mask & ImplicitScope) != 0) res += UseScope.Implicit
      if ((mask & PatMatScope) != 0) res += UseScope.PatMatTarget
      res
    }
    override def toString: String = toSet.mkString("[", ",", "]")
  }
  object UseScopeSet {
    val empty = new UseScopeSet(0)
    def apply(scopes: Iterable[UseScope]): UseScopeSet = {
      var res = 0
      scopes.foreach(s => res |= toBit(s))
      new UseScopeSet(res.toByte)
    }

    private final val DefaultScope = 1 << 1
    private final val ImplicitScope = 1 << 2
    private final val PatMatScope = 1 << 3

    private def toBit(scope: UseScope): Int = scope match {
      case UseScope.Default      => DefaultScope
      case UseScope.Implicit     => ImplicitScope
      case UseScope.PatMatTarget => PatMatScope
    }
  }
}

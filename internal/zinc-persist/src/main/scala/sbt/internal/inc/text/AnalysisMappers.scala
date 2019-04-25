/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt.internal.inc.text

import java.io.File

import sbt.internal.inc
import sbt.internal.inc.UsedName
import xsbti.compile.analysis.Stamp

case class Mapper[V](read: String => V, write: V => String)
case class ContextAwareMapper[C, V](read: (C, String) => V, write: (C, V) => String)

object Mapper {
  val forFile: Mapper[File] = Mapper(FormatCommons.stringToFile, FormatCommons.fileToString)
  val forString: Mapper[String] = Mapper(identity, identity)
  val forStamp: ContextAwareMapper[File, Stamp] =
    ContextAwareMapper((_, v) => sbt.internal.inc.Stamp.fromString(v), (_, s) => s.toString)
  val forUsedName: Mapper[UsedName] = {
    val OffsetInAscii = '!'.toInt

    def serialize(usedName: UsedName): String =
      s"${(usedName.scopes.mask.toInt + OffsetInAscii).toChar}${usedName.name}"

    def deserialize(s: String) =
      UsedName(s.tail, new inc.UsedName.UseScopeSet((s.head - OffsetInAscii).toByte))

    Mapper(deserialize, serialize)
  }

  implicit class MapperOpts[V](mapper: Mapper[V]) {
    def map[T](map: V => T, unmap: T => V) =
      Mapper[T](mapper.read.andThen(map), unmap.andThen(mapper.write))
  }
}

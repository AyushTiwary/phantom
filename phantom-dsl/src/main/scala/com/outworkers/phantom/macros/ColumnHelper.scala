/*
 * Copyright 2013 - 2017 Outworkers Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.outworkers.phantom.macros

import com.outworkers.phantom.column.AbstractColumn
import com.outworkers.phantom.macros.toolbelt.BlackboxToolbelt
import com.outworkers.phantom.{ColumnNaming, TableNaming}

import scala.reflect.macros.blackbox

trait ColumnHelper[Col <: AbstractColumn[_]] {
  def columnName(source: String): String
}

object ColumnHelper {
  def apply[Col <: AbstractColumn[_]](implicit ev: ColumnHelper[Col]): ColumnHelper[Col] = ev

  implicit def materialize[Col <: AbstractColumn[_]]: ColumnHelper[Col] = macro ColumnHelperMacro.materialize[Col]

}

@macrocompat.bundle
class ColumnHelperMacro(override val c: blackbox.Context) extends BlackboxToolbelt with RootMacro {

  import c.universe._

  /**
    * This will search the implicit scope for a [[TableNaming.Strategy]] defined.
    * If none is found, this will return the table name as is.
    *
    * @param columnName The name of the table as derived from the user input.
    * @return A new table name adjusted according to the [[TableNaming.Strategy]].
    */
  def adjustColumnName(columnName: TermName): Tree = {
    val strategy = c.inferImplicitValue(typeOf[ColumnNaming.Strategy], silent = true)

    if (strategy.isEmpty) {
      info("No ColumnNaming.Strategy found in implicit scope.")
      q"$columnName"
    } else {
      info(s"Altering table name with strategy ${showCode(strategy)}")
      val tree = q"$strategy.inferName($columnName)"

      evalTree(tree)
    }
  }

  def materialize[Col : WeakTypeTag]: Tree = {
    val currentColumnType = weakTypeOf[Col]

    q"""
        new $macroPkg.ColumnHelper[$currentColumnType] {
          override def columnName($inputTerm: $strTpe): $strTpe = ${adjustColumnName(inputTerm)}
        }
    """
  }
}
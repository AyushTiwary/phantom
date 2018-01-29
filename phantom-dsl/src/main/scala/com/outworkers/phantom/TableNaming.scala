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
package com.outworkers.phantom

import com.google.common.base.CaseFormat
import com.outworkers.phantom.builder.query.engine.CQLQuery

class NamingClass {

  abstract class Strategy(protected[this] val isCaseSensitive: Boolean = false) {
    def strategy: String

    def inferName(name: String): String

    def apply(name: String): String = inferName(name)
  }


  sealed class CamelCase(
    override protected[this] val isCaseSensitive: Boolean
  ) extends Strategy(isCaseSensitive) {

    override def strategy: String = "camel_case"

    override def inferName(name: String): String = {
      val source = CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, name)

      if (isCaseSensitive) {
        CQLQuery.escape(source)
      } else {
        source
      }
    }
  }

  sealed class SnakeCase(
    override protected[this] val isCaseSensitive: Boolean
  ) extends Strategy(isCaseSensitive) {
    override def strategy: String = "snake_case"

    override def inferName(name: String): String = {
      val source = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, name)
      if (isCaseSensitive) {
        CQLQuery.escape(source)
      } else {
        source
      }
    }
  }

  sealed class IdentityStrategy(
    override protected[this] val isCaseSensitive: Boolean
  ) extends Strategy(isCaseSensitive) {

    override def strategy: String = "identity"

    override def inferName(source: String): String = {
      if (isCaseSensitive) {
        CQLQuery.escape(source)
      } else {
        source
      }
    }
  }
}




sealed trait LowPriorityImplicits {
  implicit val tableIdentityStrategy: ColumnNaming.IdentityStrategy = new ColumnNaming.IdentityStrategy(false)
  implicit val columnIdentityStrategy: TableNaming.IdentityStrategy = new TableNaming.IdentityStrategy(false)
}


object ColumnNaming extends NamingClass with LowPriorityImplicits {
  object CamelCase {
    implicit val caseSensitive: Strategy = new CamelCase(true)
    implicit val caseInsensitive: Strategy = new CamelCase(false)
  }

  object SnakeCase {
    implicit val caseSensitive: Strategy = new SnakeCase(true)
    implicit val caseInsensitive: Strategy = new SnakeCase(false)
  }

  object Default {
    implicit val caseSensitive: Strategy = new IdentityStrategy(true)
    implicit val caseInsensitive: Strategy = new IdentityStrategy(false)
  }
}

object TableNaming extends NamingClass with LowPriorityImplicits {

  object CamelCase {
    implicit val caseSensitive: Strategy = new CamelCase(true)
    implicit val caseInsensitive: Strategy = new CamelCase(false)
  }

  object SnakeCase {
    implicit val caseSensitive: Strategy = new SnakeCase(true)
    implicit val caseInsensitive: Strategy = new SnakeCase(false)
  }

  object Default {
    implicit val caseSensitive: Strategy = new IdentityStrategy(true)
    implicit val caseInsensitive: Strategy = new IdentityStrategy(false)
  }
}

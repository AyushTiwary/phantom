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
package com.outworkers.phantom.tables.bugs

import com.outworkers.phantom.dsl._
import scala.concurrent.Future

case class User(
  id: UUID,
  email: String,
  firstName: String,
  lastName: String,
  passwordHash: String,
  createdAt: DateTime,
  updatedAt: DateTime
)

abstract class Users extends Table[Users, User] {

  object id extends UUIDColumn with PartitionKey
  object email extends StringColumn
  object first_name extends StringColumn
  object last_name extends StringColumn
  object password_hash extends StringColumn
  object created_at extends DateTimeColumn
  object updated_at extends DateTimeColumn

  def getById(id: UUID): Future[Option[User]] = {
    select.where(_.id eqs id).one()
  }

  def add(user: User): Future[UUID] = {
    store(user)
      .consistencyLevel_=(ConsistencyLevel.ALL)
      .future()
      .map(_ => user.id)
  }
}


class AppDatabase(override val connector: CassandraConnection)
  extends Database[AppDatabase](connector) {
  object users extends Users with Connector
}
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
package com.outworkers.phantom.connectors

import com.datastax.oss.driver.api.core.{CqlSession, DefaultProtocolVersion, ProtocolVersion}

trait SessionAugmenter {

  def session: CqlSession

  def protocolVersion: ProtocolVersion = {
    DefaultProtocolVersion.V5
  }

  def isNewerThan(pv: ProtocolVersion): Boolean = {
    protocolVersion.getCode > pv.getCode
  }

  def v3orNewer: Boolean = isNewerThan(DefaultProtocolVersion.V3)

  def protocolConsistency: Boolean = isNewerThan(DefaultProtocolVersion.V3)

  def v4orNewer: Boolean = isNewerThan(DefaultProtocolVersion.V4)
}

trait SessionAugmenterImplicits {
  implicit class RichSession(val session: CqlSession) extends SessionAugmenter
}

case class KeySpace(name: String)

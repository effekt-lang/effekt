/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2018-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package kiama
package util

import org.eclipse.lsp4j.services.LanguageClient

case class Product(
  uri: String,
  name: String,
  language: String,
  content: String,
  append: Boolean,
  rangeMap: Array[RangeEntry],
  rangeMapRev: Array[RangeEntry]
)

case class RangeEntry(
  source: OffsetRange,
  targets: Array[OffsetRange]
)

case class OffsetRange(
  start: Int, end: Int
)

/**
 * Extend standard language client with Monto support.
 */
trait Client extends LanguageClient {

  import org.eclipse.lsp4j.jsonrpc.services._

  @JsonNotification("monto/publishProduct")
  def publishProduct(product: Product): Unit

}

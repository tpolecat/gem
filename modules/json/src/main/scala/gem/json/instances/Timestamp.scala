// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.format._
import gem.util.Timestamp
import io.circe.{ Decoder, Encoder }
import io.circe.java8.time.{ decodeInstant, encodeInstant }

trait TimestampJson {

  implicit val TimestampEncoder: Encoder[Timestamp] = Timestamp.instant.toEncoder
  implicit val TimestampDecoder: Decoder[Timestamp] = Timestamp.instant.toDecoder

}
object timestamp extends TimestampJson
// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json

package object instances {

  object all
    extends CoordinatesJson
       with EnumeratedJson
       with EphemerisJson
       with OffsetJson
       with ProperMotionJson
       with TargetJson
       with TimeJson

}


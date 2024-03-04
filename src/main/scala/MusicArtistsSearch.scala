object MusicArtistsSearch {
  // STEP 0: Design using what we know (primitive types)
  object Version0 {
    case class Artist(
        name: String,
        genre: String,
        origin: String,
        yearsActiveStart: Int,
        isActive: Boolean,
        yearsActiveEnd: Int
    )

    def searchArtists(
        artists: List[Artist],
        genres: List[String],
        locations: List[String],
        searchByActiveYears: Boolean,
        activeAfter: Int,
        activeBefore: Int
    ): List[Artist] = artists.filter(artist =>
      (genres.isEmpty || genres.contains(artist.genre)) &&
        (locations.isEmpty || locations.contains(artist.origin)) &&
        (!searchByActiveYears || (artist.isActive || artist.yearsActiveEnd >= activeAfter) &&
          artist.yearsActiveStart <= activeBefore)
    )
  }

  // STEP 1: newtypes
  // In Scala, you could also use opaque types to encode newtypes:
  object model {
    opaque type Location = String
    object Location {
      def apply(value: String): Location       = value // <- you can use a String as a Location only in the scope of model
      extension (a: Location) def name: String = a
    }

    // Practicing newtypes
    opaque type Genre = String
    object Genre {
      def apply(value: String): Genre       = value
      extension (a: Genre) def name: String = a
    }

    opaque type YearsActiveStart = Int
    object YearsActiveStart {
      def apply(value: Int): YearsActiveStart        = value
      extension (a: YearsActiveStart) def value: Int = a
    }

    opaque type YearsActiveEnd = Int
    object YearsActiveEnd {
      def apply(value: Int): YearsActiveEnd        = value
      extension (a: YearsActiveEnd) def value: Int = a
    }
  }

  import model._
  val us: Location = Location("U.S.")
  // val wontCompile: Location = "U.S." // <- String can't be used as a Location outside of the scope of model

  object Version1 {
    case class Artist(
        name: String,
        genre: Genre,
        origin: Location,
        yearsActiveStart: YearsActiveStart,
        isActive: Boolean,
        yearsActiveEnd: YearsActiveEnd
    )

    def searchArtists(
        artists: List[Artist],
        genres: List[String],
        locations: List[String],
        searchByActiveYears: Boolean,
        activeAfter: Int,
        activeBefore: Int
    ): List[Artist] = artists.filter(artist =>
      (genres.isEmpty || genres.contains(artist.genre.name)) &&              // <- using Genre
        (locations.isEmpty || locations
          .contains(artist.origin.name)) &&                                  // <- using Location
        (!searchByActiveYears ||
          (artist.isActive || artist.yearsActiveEnd.value >= activeAfter) && // <- using YearsActiveEnd
          artist.yearsActiveStart.value <= activeBefore)                     // <- using YearsActiveStart
    )
  }

  // STEP 2a: Option type (reverted all newtypes except of origin, because we'll make them better)
  object Version2a {
    case class Artist(
        name: String,
        genre: String,
        origin: Location,
        yearsActiveStart: Int,
        yearsActiveEnd: Option[Int]
    )

    def searchArtists(
        artists: List[Artist],
        genres: List[String],
        locations: List[String],
        searchByActiveYears: Boolean,
        activeAfter: Int,
        activeBefore: Int
    ): List[Artist] = artists.filter(artist =>
      (genres.isEmpty || genres.contains(artist.genre)) &&
        (locations.isEmpty || locations.contains(artist.origin.name)) &&
        (!searchByActiveYears ||
          artist.yearsActiveEnd.forall(_ >= activeAfter) && // <- using Option.forall
          artist.yearsActiveStart <= activeBefore)
    )
  }

  // STEP 2b: new product type
  object Version2b_Data {
    case class PeriodInYears(start: Int, end: Option[Int])

    case class Artist(
        name: String,
        genre: String,
        origin: Location,
        yearsActive: PeriodInYears
    )
  }

  object Version2b_Behavior {
    import Version2b_Data._

    def searchArtists(
        artists: List[Artist],
        genres: List[String],
        locations: List[String],
        searchByActiveYears: Boolean,
        activeAfter: Int,
        activeBefore: Int
    ): List[Artist] = artists.filter(artist =>
      (genres.isEmpty || genres.contains(artist.genre)) &&
        (locations.isEmpty || locations.contains(artist.origin.name)) &&
        (!searchByActiveYears ||
          artist.yearsActive.end.forall(_ >= activeAfter) && // <- using new product type (end)
          artist.yearsActive.start <= activeBefore)          // <- using new product type (start)
    )
  }

  // STEP 3: sum type
  enum MusicGenre {
    case HeavyMetal
    case Pop
    case HardRock
  }

  import MusicGenre._

  object Version3 {
    import Version2b_Data.PeriodInYears

    case class Artist(
        name: String,
        genre: MusicGenre,
        origin: Location,
        yearsActive: PeriodInYears
    )

    def searchArtists(
        artists: List[Artist],
        genres: List[MusicGenre], // <- now we need to make sure only valid genres are searched for
        locations: List[String],
        searchByActiveYears: Boolean,
        activeAfter: Int,
        activeBefore: Int
    ): List[Artist] = artists.filter(artist =>
      (genres.isEmpty || genres.contains(artist.genre)) && // no change needed
        (locations.isEmpty || locations.contains(artist.origin.name)) &&
        (!searchByActiveYears ||
          artist.yearsActive.end.forall(_ >= activeAfter) &&
          artist.yearsActive.start <= activeBefore)
    )
  }

  // STEP 4: Algebraic Data Type (ADT) = product type + sum type
  enum YearsActive {
    case StillActive(since: Int)
    case ActiveBetween(start: Int, end: Int)
  }

  import YearsActive._

  object Version4_Data {
    case class Artist(name: String, genre: MusicGenre, origin: Location, yearsActive: YearsActive)
  }

  object Version4_Behavior {
    import Version4_Data._

    def wasArtistActive(artist: Artist, yearStart: Int, yearEnd: Int): Boolean = artist.yearsActive match {
      case StillActive(since)        => since <= yearEnd
      case ActiveBetween(start, end) => start <= yearEnd && end >= yearStart
    }

    def searchArtists(
        artists: List[Artist],
        genres: List[MusicGenre],
        locations: List[Location],
        searchByActiveYears: Boolean,
        activeAfter: Int,
        activeBefore: Int
    ): List[Artist] = artists.filter(artist =>
      (genres.isEmpty || genres.contains(artist.genre)) &&
        (locations.isEmpty || locations.contains(artist.origin)) &&
        (!searchByActiveYears || wasArtistActive(artist, activeAfter, activeBefore))
    )
  }

  object FirstNewRequirement { // Practicing pattern matching
    import Version4_Data._

    def activeLength(artist: Artist, currentYear: Int): Int = artist.yearsActive match {
      case StillActive(since)        => currentYear - since
      case ActiveBetween(start, end) => end - start
    }
  }

  // STEP 5: modeling behaviors
  object Version5 {
    import Version4_Data._
    import Version4_Behavior.wasArtistActive

    // Modeling conditions as ADTs:
    enum SearchCondition {
      case SearchByGenre(genres: List[MusicGenre])
      case SearchByOrigin(locations: List[Location])
      case SearchByActiveYears(start: Int, end: Int)
    }

    import SearchCondition._

    def searchArtists(
        artists: List[Artist],
        requiredConditions: List[SearchCondition]
    ): List[Artist] = artists.filter(artist =>
      requiredConditions.forall(condition =>
        condition match {
          case SearchByGenre(genres)           => genres.contains(artist.genre)
          case SearchByOrigin(locations)       => locations.contains(artist.origin)
          case SearchByActiveYears(start, end) => wasArtistActive(artist, start, end)
        }
      )
    )
  }

  // Checking whether the design is good by trying to implement even more new requirements
  object NewRequirements {
    case class PeriodInYears(start: Int, end: Int)

    enum YearsActive {
      case StillActive(since: Int, previousPeriods: List[PeriodInYears])
      case ActiveInPast(periods: List[PeriodInYears])
    }

    case class Artist(name: String, genre: MusicGenre, origin: Location, yearsActive: YearsActive)

    enum SearchCondition {
      case SearchByGenre(genres: List[MusicGenre])
      case SearchByOrigin(locations: List[Location])
      case SearchByActiveYears(period: PeriodInYears)
      case SearchByActiveLength(howLong: Int, until: Int)
    }

    import SearchCondition._, YearsActive._

    def periodOverlapsWithPeriods(checkedPeriod: PeriodInYears, periods: List[PeriodInYears]): Boolean =
      periods.exists(p => p.start <= checkedPeriod.end && p.end >= checkedPeriod.start)

    def wasArtistActive(artist: Artist, searchedPeriod: PeriodInYears): Boolean = artist.yearsActive match {
      case StillActive(since, previousPeriods) =>
        since <= searchedPeriod.end || periodOverlapsWithPeriods(searchedPeriod, previousPeriods)
      case ActiveInPast(periods)               => periodOverlapsWithPeriods(searchedPeriod, periods)
    }

    def activeLength(artist: Artist, currentYear: Int): Int = {
      val periods = artist.yearsActive match {
        case StillActive(since, previousPeriods) => previousPeriods.appended(PeriodInYears(since, currentYear))
        case ActiveInPast(periods)               => periods
      }
      periods.map(p => p.end - p.start).foldLeft(0)((x, y) => x + y)
    }

    def searchArtists(artists: List[Artist], requiredConditions: List[SearchCondition]): List[Artist] = artists.filter(
      artist =>
        requiredConditions.forall(condition =>
          condition match {
            case SearchByGenre(genres)                => genres.contains(artist.genre)
            case SearchByOrigin(locations)            => locations.contains(artist.origin)
            case SearchByActiveYears(period)          => wasArtistActive(artist, period)
            case SearchByActiveLength(howLong, until) => activeLength(artist, until) >= howLong
          }
        )
    )
  }
}

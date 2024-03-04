import org.scalatest.funsuite.AnyFunSuite
import MusicArtistsSearch.*
import YearsActive.*
import MusicGenre.*

class MusicArtistsSearchTest extends AnyFunSuite {
  test("Version0") {
    import Version0._

    val artists = List(
      Artist("Metallica", "Heavy Metal", "U.S.", 1981, true, 0),
      Artist("Led Zeppelin", "Hard Rock", "England", 1968, false, 1980),
      Artist("Bee Gees", "Pop", "England", 1958, false, 2003)
    )

    assert(searchArtists(artists, List("Pop"), List("England"), true, 1950, 2022) == List(Artist(
      "Bee Gees",
      "Pop",
      "England",
      1958,
      false,
      2003
    )))

    assert(searchArtists(artists, List.empty, List("England"), true, 1950, 2022) ==
      List(
        Artist("Led Zeppelin", "Hard Rock", "England", 1968, false, 1980),
        Artist("Bee Gees", "Pop", "England", 1958, false, 2003)
      ))

    assert(searchArtists(artists, List.empty, List.empty, true, 1981, 2003) ==
      List(
        Artist("Metallica", "Heavy Metal", "U.S.", 1981, true, 0),
        Artist("Bee Gees", "Pop", "England", 1958, false, 2003)
      ))

    assert(searchArtists(artists, List.empty, List("U.S."), false, 0, 0) ==
      List(
        Artist("Metallica", "Heavy Metal", "U.S.", 1981, true, 0)
      ))

    assert(searchArtists(artists, List.empty, List.empty, false, 2019, 2022) ==
      List(
        Artist("Metallica", "Heavy Metal", "U.S.", 1981, true, 0),
        Artist("Led Zeppelin", "Hard Rock", "England", 1968, false, 1980),
        Artist("Bee Gees", "Pop", "England", 1958, false, 2003)
      ))

    assert(searchArtists(artists, List.empty, List("U.S."), true, 1950, 1959) == List.empty)

    assert(searchArtists(artists, List.empty, List.empty, true, 1950, 1979) ==
      List(
        Artist("Led Zeppelin", "Hard Rock", "England", 1968, false, 1980),
        Artist("Bee Gees", "Pop", "England", 1958, false, 2003)
      ))

    assert(searchArtists(artists, List.empty, List.empty, true, 1950, 1959) ==
      List(
        Artist("Bee Gees", "Pop", "England", 1958, false, 2003)
      ))

    assert(searchArtists(artists, List("Heavy Metal"), List.empty, true, 2019, 2022) ==
      List(
        Artist("Metallica", "Heavy Metal", "U.S.", 1981, true, 0)
      ))
  }

  test("Version1") {
    import model.*
    import Version1._

    val artists = List(
      Artist("Metallica", Genre("Heavy Metal"), Location("U.S."), YearsActiveStart(1981), true, YearsActiveEnd(0)),
      Artist(
        "Led Zeppelin",
        Genre("Hard Rock"),
        Location("England"),
        YearsActiveStart(1968),
        false,
        YearsActiveEnd(1980)
      ),
      Artist("Bee Gees", Genre("Pop"), Location("England"), YearsActiveStart(1958), false, YearsActiveEnd(2003))
    )

    assert(searchArtists(artists, List("Pop"), List("England"), true, 1950, 2022) ==
      List(Artist("Bee Gees", Genre("Pop"), Location("England"), YearsActiveStart(1958), false, YearsActiveEnd(2003))))
  }

  test("Version2a") {
    import model.*
    import Version2a._

    val artists = List(
      Artist("Metallica", "Heavy Metal", Location("U.S."), 1981, None),
      Artist("Led Zeppelin", "Hard Rock", Location("England"), 1968, Some(1980)),
      Artist("Bee Gees", "Pop", Location("England"), 1958, Some(2003))
    )

    assert(searchArtists(artists, List("Pop"), List("England"), true, 1950, 2022) ==
      List(Artist("Bee Gees", "Pop", Location("England"), 1958, Some(2003))))
  }

  test("Version2b") {
    import model.*
    import Version2b_Data._
    import Version2b_Behavior._

    val artists = List(
      Artist("Metallica", "Heavy Metal", Location("U.S."), PeriodInYears(1981, None)),
      Artist("Led Zeppelin", "Hard Rock", Location("England"), PeriodInYears(1968, Some(1980))),
      Artist("Bee Gees", "Pop", Location("England"), PeriodInYears(1958, Some(2003)))
    )

    assert(searchArtists(artists, List("Pop"), List("England"), true, 1950, 2022) ==
      List(Artist("Bee Gees", "Pop", Location("England"), PeriodInYears(1958, Some(2003)))))
  }

  test("Version3") {
    import model.*
    import Version2b_Data.PeriodInYears
    import Version3._

    val artists = List(
      Artist("Metallica", HeavyMetal, Location("U.S."), PeriodInYears(1981, None)),
      Artist("Led Zeppelin", HardRock, Location("England"), PeriodInYears(1968, Some(1980))),
      Artist("Bee Gees", Pop, Location("England"), PeriodInYears(1958, Some(2003)))
    )

    assert(searchArtists(artists, List(Pop), List("England"), true, 1950, 2022) == List(Artist(
      "Bee Gees",
      Pop,
      Location("England"),
      PeriodInYears(1958, Some(2003))
    )))
  }

  test("Version4") {
    import model.Location
    import Version4_Data._
    import Version4_Behavior._

    val artists = List(
      Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(since = 1981)),
      Artist("Led Zeppelin", HardRock, Location("England"), ActiveBetween(1968, 1980)),
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    )

    assert(searchArtists(artists, List(Pop), List(Location("England")), true, 1950, 2022) == List(
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    ))
  }

  test("FirstNewRequirement") {
    import model.Location
    import Version4_Data._
    import FirstNewRequirement._

    assert(activeLength(Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981)), 2022) == 41)
    assert(activeLength(Artist("Led Zeppelin", HardRock, Location("England"), ActiveBetween(1968, 1980)), 2022) == 12)
    assert(activeLength(Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003)), 2022) == 45)
  }

  test("Version5") {
    import model.*
    import Version4_Data._
    import Version5._
    import SearchCondition._, YearsActive._

    val artists = List(
      Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(since = 1981)),
      Artist("Led Zeppelin", HardRock, Location("England"), ActiveBetween(1968, 1980)),
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    )

    assert(searchArtists(
      artists,
      List(
        SearchByGenre(List(Pop)),
        SearchByOrigin(List(Location("England"))),
        SearchByActiveYears(1950, 2022)
      )
    ) == List(
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    ))

    assert(searchArtists(
      artists,
      List(
        SearchByActiveYears(1950, 2022)
      )
    ) == List(
      Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(since = 1981)),
      Artist("Led Zeppelin", HardRock, Location("England"), ActiveBetween(1968, 1980)),
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    ))

    assert(searchArtists(
      artists,
      List(
        SearchByGenre(List(Pop)),
        SearchByOrigin(List(Location("England")))
      )
    ) == List(
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    ))

    assert(searchArtists(artists, List.empty) == artists)

    assert(searchArtists(
      artists,
      List(
        SearchByActiveYears(1983, 2003)
      )
    ) == List(
      Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(since = 1981)),
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    ))

    assert(searchArtists(
      artists,
      List(
        SearchByGenre(List(HeavyMetal)),
        SearchByActiveYears(2019, 2022)
      )
    ) == List(
      Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(since = 1981))
    ))

    assert(searchArtists(
      artists,
      List(
        SearchByActiveYears(1950, 1959)
      )
    ) == List(
      Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
    ))

    assert(searchArtists(
      artists,
      List(
        SearchByOrigin(List(Location("U.S."))),
        SearchByActiveYears(1950, 1959)
      )
    ) == List.empty)
  }

  test("NewRequirements") {
    import model.Location
    import NewRequirements.*
    import SearchCondition._, YearsActive._

    val artists = List(
      Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981, List.empty)),
      Artist("Led Zeppelin", HardRock, Location("England"), ActiveInPast(List(PeriodInYears(1968, 1980)))),
      Artist(
        "Bee Gees",
        Pop,
        Location("England"),
        ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByGenre(List(Pop)),
          SearchByOrigin(List(Location("England"))),
          SearchByActiveYears(PeriodInYears(1950, 2022))
        )
      ) == List(
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByOrigin(List(Location("England"))),
          SearchByActiveYears(PeriodInYears(1950, 2022))
        )
      ) == List(
        Artist("Led Zeppelin", HardRock, Location("England"), ActiveInPast(List(PeriodInYears(1968, 1980)))),
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByActiveYears(PeriodInYears(1950, 2022))
        )
      ) == List(
        Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981, List.empty)),
        Artist("Led Zeppelin", HardRock, Location("England"), ActiveInPast(List(PeriodInYears(1968, 1980)))),
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByActiveYears(PeriodInYears(1983, 2003))
        )
      ) == List(
        Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981, List.empty)),
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByActiveYears(PeriodInYears(2019, 2022))
        )
      ) == List(
        Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981, List.empty))
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByActiveYears(PeriodInYears(1950, 1959))
        )
      ) == List(
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByActiveLength(48, 2022)
        )
      ) == List(
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByActiveLength(48, 2022)
        )
      ) == List(
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByOrigin(List(Location("U.S."))),
          SearchByActiveLength(48, 2022)
        )
      ) == List.empty
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByOrigin(List(Location("U.S."))),
          SearchByActiveLength(40, 2022)
        )
      ) == List(Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981, List.empty)))
    )

    assert(
      searchArtists(
        artists,
        List(
          SearchByOrigin(List(Location("U.S."), Location("England"))),
          SearchByActiveLength(40, 2022)
        )
      ) == List(
        Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981, List.empty)),
        Artist(
          "Bee Gees",
          Pop,
          Location("England"),
          ActiveInPast(List(PeriodInYears(1958, 2003), PeriodInYears(2009, 2012)))
        )
      )
    )
  }
}

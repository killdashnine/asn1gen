package test.org.asn1gen.extra

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array
    ( classOf[TestExtraByte]
    , classOf[TestExtraString]
    )
)
class Test_

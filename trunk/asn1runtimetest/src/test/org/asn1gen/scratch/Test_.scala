package test.org.asn1gen.scratch

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
 
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array
    ( classOf[TestExceptionPerformance]
    , classOf[TestStream]
    )
)
class Test_

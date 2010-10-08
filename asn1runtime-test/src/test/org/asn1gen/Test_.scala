package test.org.asn1gen

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
 
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array
    ( classOf[runtime.Test_]
    , classOf[scratch.Test_]
    )
)
class Test_ {
}

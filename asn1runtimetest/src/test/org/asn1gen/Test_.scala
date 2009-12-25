package test.org.asn1gen

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
 
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array
    ( classOf[extra.Test_]
    , classOf[runtime.Test_]
    )
)
class Test_ {
}

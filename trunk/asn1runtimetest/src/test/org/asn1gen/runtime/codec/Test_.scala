package test.org.asn1gen.runtime.codec

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
 
@RunWith(classOf[Suite])
@Suite.SuiteClasses(Array
    ( classOf[TestBerDecoder]
    , classOf[TestOctetWindow]
    , classOf[TestSpecsRunner]
    )
)
class Test_ {
}

import org.jmock.Expectations;
import org.jmock.integration.junit4.JUnitRuleMockery;

import org.junit.Rule;
import org.junit.Test;
import org.junit.Ignore;

import static org.junit.Assert.assertEquals;

// JMock generates mocks from interfaces, not classes
interface IOracle {
  int getAnswer();
  int setAnswer(int i);
}

class SUT {
  private final IOracle oracle;

  //constructor (receives DOC)
  public SUT(IOracle o) {
    oracle = o;
  }

  //sut_method (uses DOC)
  public int askTheQuestion() {
    return oracle.getAnswer();
  }

  public int setAnswer(int i) {
    return oracle.setAnswer(i);
  }
}

public class JMockTest
{
  @Rule public final JUnitRuleMockery context = new JUnitRuleMockery();
  final IOracle oracle = context.mock(IOracle.class);

  @Test
  public void satisfiesExpectations_OracleUsedOnce () {
    //final IOracle oracle = context.mock(IOracle.class);

    // Set up test and install the mock
    SUT sut = new SUT(oracle);

    // Specify the expectations
    context.checking(new Expectations() {{
      allowing(oracle).getAnswer();
      allowing(oracle).getAnswer();
      allowing(oracle).setAnswer(43);
    }});

    // Exercise the SUT
    sut.askTheQuestion();
    sut.askTheQuestion();
    sut.setAnswer(43);
  }

  @Test
  @Ignore
  public void doesNotSatisfyExpectations_OracleUsedOnce () {
    SUT sut = new SUT(oracle);

    // Specify the expectations
    context.checking(new Expectations() {{
      oneOf(oracle).getAnswer();
    }});

    // Exercise the SUT
    sut.askTheQuestion();
    //sut.askTheQuestion(); // BUG IN TEST: This will fail verification
  }
}

package test;

import se.sics.prologbeans.*;
import java.io.*;

/**
 * Describe class TestFE here.
 *
 *
 * Created: Tue May 19 09:42:22 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class TestFE {

  private PrologSession session = new PrologSession();

  /**
   * Creates a new <code>TestFE</code> instance.
   *
   */
  public TestFE() {
    session.setTimeout(320000);
  }
  
  void process(String inputFilename, String outputFilename)
  {
    try {
      StringBuffer sb = new StringBuffer();
      BufferedReader br = new BufferedReader(new FileReader(inputFilename));
      String line;
      while ((line = br.readLine()) != null) {
	sb.append(line).append("\n");
      }
      br.close();
      String input = sb.toString();
      System.out.println("input: " + input);
      Bindings bindings = new Bindings().bind("Input",
					      "\"" + input + "\".");
      QueryAnswer answer =
        session.executeQuery("process_string(Input,Output)", bindings);
      Term result = answer.getValue("Output");
      if (result != null) {
        System.out.println(input + " = " + result + '\n');
	PrintWriter pw = new PrintWriter(outputFilename);
	pw.println(input + " = " + result + '\n');
	pw.close();
      } else {
	System.out.println("Error: " + answer.getError() + "\n");
      }
    } catch (Exception e) {
      System.out.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  public static void main(String[] args) {
    TestFE frontEnd = new TestFE();
    
    if (args.length < 2) {
      System.out.println("usage: TestFE <inputfile> <outputfile>");
      System.exit(0);
    }
    frontEnd.process(args[0], args[1]);
  }
}

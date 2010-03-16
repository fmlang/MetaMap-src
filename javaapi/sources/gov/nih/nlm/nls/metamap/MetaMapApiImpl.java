package gov.nih.nlm.nls.metamap;

import java.io.*;
import java.util.List;
import se.sics.prologbeans.*;

/**
 * The default implementation of the MetaMapApi interface.
 *
 * <p>
 * Created: Wed Apr 29 14:59:15 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class MetaMapApiImpl implements MetaMapApi {
  private PrologSession session = new PrologSession();


  public MetaMapApiImpl() {
    session.setTimeout(150000);
  }

  /** @param timeout time in milliseconds to wait for prolog server before timing out. */
  public MetaMapApiImpl(int timeout) {
    session.setTimeout(timeout);
  }

  public PrologSession getSession() {
    return this.session;
  }

  public String getOptions() {
    Term options = null;
    try {
      QueryAnswer answer =
        session.executeQuery("get_options(AllOptions)");
      options = answer.getValue("AllOptions");
    } catch (Exception e) {
      System.out.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
    return options.toString();
  }

  public void setOptions(String optionString) {
    try {
      Bindings bindings = new Bindings().bind("Options", optionString);
      System.out.println("bindings: " + bindings.toString());
      QueryAnswer answer =
        session.executeQuery("set_options(Options)", bindings);
      System.out.println("answer: " + answer.toString());
    } catch (Exception e) {
      System.out.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  public void setOptions(List<String> options) {
    StringBuffer optionStringBuf = new StringBuffer();
    optionStringBuf.append(options.get(0));
    for (String option: options.subList(1,options.size() - 1)) {
      optionStringBuf.append(',').append(option);
    }
    this.setOptions(optionStringBuf.toString());
  }

  public void unsetOptions(String optionString) { }

  /** Process a ASCII text file of one or more documents - unicode
   * (utf8) is not supported
   * @param inputFilename the filename of a file of documents
   * @return a result instance
   */
  public Result processFile(String inputFilename) {
    Result newResult = null;
    try {
      StringBuffer sb = new StringBuffer();
      BufferedReader br = new BufferedReader(new FileReader(inputFilename));
      String line;
      while ((line = br.readLine()) != null) {
	sb.append(line).append("\n");
      }
      br.close();
      String input = sb.toString();
      // System.out.println("input: " + input);
      Bindings bindings = new Bindings().bind("Input",
					      "\"" + input + "\"");
      QueryAnswer answer =
        session.executeQuery("process_string(Input,Output)", bindings);
      Term result = answer.getValue("Output");
      if (result != null) {
        newResult = new ResultImpl(result);
      } else {
	System.out.println("Error: " + answer.getError() + "\n");
      }
    } catch (Exception e) {
      System.out.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }

    return newResult;
  }

  /** Process a ASCII text stream reader of one or more documents - unicode (utf8) is not supported
   * @param inputReader a reader stream of documents
   * @return a result instance
   */
  public Result processReader(Reader inputReader) {
    Result newResult = null;
    try {
      StringBuffer sb = new StringBuffer();
      BufferedReader br = new BufferedReader(inputReader);
      String line;
      while ((line = br.readLine()) != null) {
	sb.append(line).append("\n");
      }
      String input = sb.toString();
      // System.out.println("input: " + input);
      Bindings bindings = new Bindings().bind("Input",
					      "\"" + input + "\".");
      QueryAnswer answer =
        session.executeQuery("process_string(Input,Output)", bindings);
      Term result = answer.getValue("Output");
      if (result != null) {
        newResult = new ResultImpl(result);
      } else {
	System.out.println("Error: " + answer.getError() + "\n");
      }
    } catch (Exception e) {
      System.out.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
    return newResult;
  }

  /** process a string containing one or more documents - unicode (utf8) is not supported
   * @param aString a file of documents
   * @return a result instance
   */
  public Result processString(String aString) {
    Result newResult = null;
    try {
      System.out.println("aString: " + aString);
      Bindings bindings = new Bindings().bind("E", aString);
      System.out.println("bindings: " + bindings.toString());
      QueryAnswer answer =
	session.executeQuery("process_string(E,Output)", bindings);
      Term result = answer.getValue("Output");

      if (result != null) {
	System.out.println("answer: " + result.toString());
        newResult = new ResultImpl(result);
      } else {
        System.err.println("Error: " + answer.getError() + "\n");
      }
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
      e.printStackTrace();
    }
    return newResult;
  }
}

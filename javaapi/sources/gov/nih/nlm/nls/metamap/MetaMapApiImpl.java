package gov.nih.nlm.nls.metamap;

import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
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

  /** Instantiate api using the default timeout. */
  public MetaMapApiImpl() {
    try {
      this.session.setTimeout(DEFAULT_TIMEOUT);
      this.session.connect();
      /* only -q should set initially */
      this.resetOptions();
      // this.unsetOptions(this.getOptions());
      // this.setOptions("['-q']");
    } catch (IOException e) {
      System.err.println("Error when communicating with Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /** Instantiate api using specified timeout.
   * @param timeout time in milliseconds to wait for prolog server before timing out.
   */
  public MetaMapApiImpl(int timeout) {
    try {
      this.session.setTimeout(timeout);
      // /* only -q should set initially */
      this.session.connect();
      this.resetOptions();
      // // this.unsetOptions(this.getOptions());
      // this.setOptions("['-q']");
    } catch (IOException e) {
      System.err.println("Error when communicating with Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /** Instantiate api using MetaMap server specified by hostname 
   * @param serverHostname hostname of metamap server
   */
  public MetaMapApiImpl(String serverHostname) {
    try {
      this.session.setTimeout(DEFAULT_TIMEOUT);
      this.session.connect();
      /* only -q should set initially */
      this.resetOptions();
      // this.unsetOptions(this.getOptions());
      // this.setOptions("['-q']");
      session.setHost(serverHostname);
    } catch (IOException e) {
      System.err.println("Error when communicating with Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /** Instantiate api using MetaMap server specified by hostname 
   * @param serverHostname hostname of metamap server
   * @param port port of metamap server
   */
  public MetaMapApiImpl(String serverHostname, int port) {
    try {
      this.session.setTimeout(DEFAULT_TIMEOUT);
      this.session.connect();
      /* only -q should set initially */
      this.resetOptions();
      // this.unsetOptions(this.getOptions());
      // this.setOptions("['-q']");
      session.setPort(port);
    } catch (IOException e) {
      System.err.println("Error when communicating with Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /**
   * Instantiate api using MetaMap server specified by hostname with specified timeout.
   * @param serverHostname hostname of metamap server 
   * @param port port metamap server
   * @param timeout time in milliseconds to wait for prolog server before timing out.
   */
  public MetaMapApiImpl(String serverHostname, int port, int timeout) {
    try {
      this.session.setTimeout(timeout);
      this.session.connect();
      // /* only -q should set initially */
      this.resetOptions();
      // // this.unsetOptions(this.getOptions());
      // this.setOptions("['-q']");
      session.setHost(serverHostname);
      session.setPort(port);
    } catch (IOException e) {
      System.err.println("Error when communicating with Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /** 
   * Return Prolog session associated with this instance of the API. 
   * @return Prolog session.
   */
  public PrologSession getSession() {
    return this.session;
  }

  /**
   * Set time for api prolog session.
   * @param timeout time in milliseconds to wait for prolog server before timing out.
   */
  public void setTimeout(int timeout) {
    session.setTimeout(timeout);
  }

  /**
   * Use MetaMap server on specified port
   * @param port of MetaMap server to use.
   */
  public void setPort(int port) {
    session.setPort(port);
  }

  /**
   * Use MetaMap server on specified host
   * @param hostname hostname of non-local metamap server
   */
  public void setHost(String hostname) {
    session.setHost(hostname);
  }

  /** Get the server's current option settings. 
   * @return string containing long format of options.
   */
  public String getOptions() {
    PBTerm options = null;
    try {
      QueryAnswer answer =
        session.executeQuery("get_options(AllOptions)");
      options = answer.getValue("AllOptions");
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
    return options.toString();
  }

  /**
   * Set MetaMap server options using a string of form:
   *
   * <pre>
   *    "-option1 optional-argument1 -option2 optional-argument2"
   * </pre>
   * examples:
   * <pre>
   *    "-yD" or "-y -D" or "-J SNOMEDCT" or "--restrict_to_sources SNOMEDCT"
   *  </pre>
   *
   * @param optionString a string of MetaMap options
   */
  public void setOptions(String optionString) {
    String[] options = optionString.split(" ");
    setOptions(options);
  }

  /**    
   * Set MetaMap server options using array of form:
   *
   * <pre>
   *  ["-y", "-D"] or ["-yD"]
   * </pre>
   * @param options an array of  options
   */
  public void setOptions(String[] options) {
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    for ( int i = 0; i < options.length; i++ ) {
      sb.append("'").append(options[i]).append("'");
      if (i < (options.length - 1)) sb.append(",");
    }
    sb.append("]");
    this.invokeSetOptions(sb.toString());
  }

  /**
   * Set options using list of form:
   *
   * <pre>
   *  ["-y", "-D"] or ["-yD"]
   * </pre>
   *
   * @param options a list of  options
   */
  public void setOptions(List<String> options) {
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    for (Iterator<String> optIter = options.iterator(); optIter.hasNext(); ) {
      sb.append("'").append(optIter.next()).append("'");
      if (optIter.hasNext()) sb.append(",");
    }
    sb.append("]");
    this.invokeSetOptions(sb.toString());
  }

  /**
   * Set MetaMap server options.
   *
   * This method sends preprocessed options to MetaMap server.  The
   * parameter optionListString is in the form of:
   * <pre>
   *   "[" + "'" + option + "'" + "," + + "'" + option + "'" + ... "]"
   * </pre>
   * E.G.:
   * <pre>
   *    optionListString = "['-y','-D','-i']"; 
   * </pre>
   *  or:
   * <pre>
   *    optionListString = "['-yDi']"; 
   * </pre>
   * sets options -y -D and -i.
   *   
   * @param optionListString a string of MetaMap options
   */
  public void invokeSetOptions(String optionListString) {
  try {
      Bindings bindings = new Bindings().bind("Options", optionListString);
      // System.out.println("bindings: " + bindings.toString());
      QueryAnswer answer =
        session.executeQuery("set_options(Options)", bindings);
      // System.out.println("answer: " + answer.toString());
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /**
   * Un-set options using a string of form:
   *   * <pre>
   *    "-option1 optional-argument1 -option2 optional-argument2"
   * </pre>
   * E.G.:
   * <pre>
   *    "-yD" or "-y -D" or 
   *  </pre>
   *
   * @param optionString a string of MetaMap options
   */
  public void unsetOptions(String optionString) {
    try {
      Bindings bindings = new Bindings().bind("Options", optionString);
      // System.out.println("bindings: " + bindings.toString());
      QueryAnswer answer =
        session.executeQuery("unset_options(Options)", bindings);
      // System.out.println("answer: " + answer.toString());
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /**
   * Un-set options using list of form:
   *
   * <pre>
   *  ["-y", "-D"] or ["-yD"]
   * </pre>
   *
   * @param options a list of options
   */
  public void unsetOptions(List<String> options) {
    try {
      StringBuffer sb = new StringBuffer();
      sb.append("[");
      for (Iterator<String> optIter = options.iterator(); optIter.hasNext(); ) {
	sb.append("'").append(optIter.next()).append("'");
	  if (optIter.hasNext()) sb.append(",");
      }
      sb.append("]");
      Bindings bindings = new Bindings().bind("Options", sb.toString());
      // System.out.println("bindings: " + bindings.toString());
      QueryAnswer answer =
        session.executeQuery("unset_options(Options)", bindings);
      // System.out.println("answer: " + answer.toString());
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
 }

  /** Reset options to defaults */
 public
 void resetOptions() {
    try {
      QueryAnswer answer =
        session.executeQuery("reset_options");
      // System.out.println("answer: " + answer.toString());
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
  }

  /** process a string containing one or more documents - unicode (utf8) is not supported
   * @param aString a file of documents
   * @return a list of one of more result instances, which may be empty.
   */
  public List<Result> processCitationsFromString(String aString) {
    List<Result> newResultList = new ArrayList<Result>();
    /* split string into multiple citations for content separated by blank lines. */
    String[] citations = aString.split("\n[\\s]*\n");
    for (int i = 0; i < citations.length; i++) {
      Result newResult = null;
      try {
	// System.out.println("\n*** citation[" + i + "]: " + citations[i]);
	Bindings bindings = new Bindings().bind("E", citations[i]);
	// System.out.println("bindings: " + bindings.toString());
	QueryAnswer answer =
	  session.executeQuery("process_string(E,Output)", bindings);
	PBTerm result = answer.getValue("Output");
	
	if (result != null) {
	  // System.out.println("answer: " + result.toString());
	  newResult = new ResultImpl(result, citations[i]);
	  newResultList.add(newResult);
	} else {
	  System.err.println("Error: " + answer.getError() + "\n");
	}
      } catch (Exception e) {
	System.err.println("Error when querying Prolog Server: " +
			   e.getMessage() + '\n');
	e.printStackTrace();
      }
    }
    return newResultList;
  }

  /** Process a ASCII text stream reader of one or more documents - unicode (utf8) is not supported
   * @param inputReader a reader stream of documents
   * @return a list of one or more result instances
   */
  public List<Result> processCitationsFromReader(Reader inputReader) {
    try {
      StringBuffer sb = new StringBuffer();
      BufferedReader br = new BufferedReader(inputReader);
      String line;
      while ((line = br.readLine()) != null) {
	sb.append(line).append("\n");
      }
      return processCitationsFromString(sb.toString());
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
    return null;
  }
  
  /** Process a ASCII text file of one or more documents - unicode (utf8) is not supported
   * @param inputFilename the filename of a file of documents
   * @return a list of one or more result instances
   */
  public List<Result> processCitationsFromFile(String inputFilename) {
    try {
      return processCitationsFromReader(new FileReader(inputFilename));
    } catch (Exception e) {
      System.err.println("Error when querying Prolog Server: " +
			 e.getMessage() + '\n');
    }
    return null;
  }
  

  protected void finalize() {
    System.err.println("finalize");
    System.err.flush();
    resetOptions();
  }
}

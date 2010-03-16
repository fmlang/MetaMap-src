package gov.nih.nlm.nls.metamap;

import java.io.Reader;
import java.util.List;

/**
 * This provides a generic interface to access the MetaMap server.
 * <p>
 * Created: Wed Apr 29 14:09:20 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface MetaMapApi {

  String getOptions();

  /**
   * Set MetaMap server options.
   * a string of MetaMap options of the form: "<single char
   * option>[,<single char option>]".  For example: "q,y" sets machine
   * output and word sense disambigutation as options.  If no options
   * are set metamap defaults to using machine output only.
   * @param optionString a string of MetaMap options
   */
  void setOptions(String optionString);

  /**
   * Set MetaMap server options.
   * @param options a list of one-character options
   */
  void setOptions(List<String> options);

  /** */
  void unsetOptions(String optionString);


  /** Process a ASCII text file of one or more documents - unicode (utf8) is not supported
   * @param aFilename the filename of a file of documents
   * @return a result instance
   */
  Result processFile(String aFilename);


  /** Process a ASCII text stream reader of one or more documents - unicode (utf8) is not supported
   * @param inputReader a reader stream of documents
   * @return a result instance
   */
  Result processReader(Reader inputReader);

  /** process a string containing one or more documents - unicode (utf8) is not supported
   * @param aString a file of documents
   * @return a result instance
   */
  Result processString(String aString);

}

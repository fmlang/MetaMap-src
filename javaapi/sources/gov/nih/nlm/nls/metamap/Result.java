package gov.nih.nlm.nls.metamap;

import java.util.List;
import java.io.PrintStream;
import se.sics.prologbeans.PBList;

/**
 * A container for the machine output representation of a MetaMap
 * result.
 *
 *<p>
 * Created: Wed Apr 29 15:44:14 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public interface Result {
  /**
   * Return a prolog beans list of terms representing machine output.
   * @return prolog beans list of machine output terms */
  PBList getMMOPBlist();
  /**
   * Return the raw string representation of MetaMap machine output.
   * @return a <code>String</code> containing the result as MetaMap
   * machine output.
   */
  String getMachineOutput();
  /**
   * Print machine output to stream.
   * @param out stream to send output.
   */
  void traverse(PrintStream out);
  /**
   * Return a list of Acronym and Abbreviations objects.
   * @return a list of <code>AcronymAbbrevs</code> objects */
  List<AcronymsAbbrevs> getAcronymsAbbrevs() throws Exception ;
  /**
   * Return a list of Negation objects.
   * @return a list of <code>Negation</code> objects */
  List<Negation> getNegations() throws Exception ;
  /**
   * Return a list of Utterance objects.
   * @return a list of <code>Utterance</code> objects */
  List<Utterance> getUtteranceList() throws Exception ;
  /** 
   * Return a list of <code>PCM</code> (Phrase/Candidate/Mapping)
   * objects starting at start in machine output.  It is recommended
   * to use the <code>Utterance</code> implementation of getPCMList
   * rather than this one.
   *
   * @param utterancePosition position of relevant utterance
   * @return list of Phrase/Candidates/Mappings (PCM) objects
   */
  List<PCM> getPCMList(int utterancePosition) throws Exception ;
}

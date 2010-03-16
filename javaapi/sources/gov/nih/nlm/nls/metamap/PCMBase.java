package gov.nih.nlm.nls.metamap;

import java.util.List;
import java.util.ArrayList;
import se.sics.prologbeans.*;

/**
 * Implementation of container for a Phrase, Candidates, and Mappings set (PCM).
 *
 * <p>
 * Created: Thu May 21 17:50:44 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class PCMBase implements PCM {
  Term phraseTerm;
  Term candidatesTerm;
  Term mappingsTerm;

  /**
   * Creates a new <code>PCMBase</code> instance.
   *
   */
  public PCMBase(Term aPhraseTerm,
		 Term aCandidatesTerm,
		 Term aMappingsTerm) {
    this.phraseTerm      = aPhraseTerm;
    this.candidatesTerm  = aCandidatesTerm;
    this.mappingsTerm    = aMappingsTerm;
  }

  // Implementation of gov.nih.nlm.nls.metamap.PCM

  /**
   * Describe <code>getPhrase</code> method here.
   *
   * @return a <code>Phrase</code> value
   */
  public final Phrase getPhrase() throws Exception {
    return new PhraseImpl(this.phraseTerm);
  }

  /**
   * Describe <code>getCandidates</code> method here.
   *
   * @return a <code>List</code> value
   */
  public final List<Ev> getCandidates() throws Exception  {
    List<Ev> evList = new ArrayList<Ev>();
    PBList prologList = (PBList)this.candidatesTerm.getArgument(1);
    for (int i = 1; i <= prologList.getLength(); i++) {
      evList.add(new EvImpl(prologList.getTermAt(i)));
    }
    return evList;
  }

  /**
   * Describe <code>getMappings</code> method here.
   *
   * @return a <code>List</code> value
   * @exception Exception if an error occurs
   */
  public final List<Map> getMappings() throws Exception {
    List<Map> mapList = new ArrayList<Map>();
    PBList prologList = (PBList)this.mappingsTerm.getArgument(1);
    for (int i = 1; i <= prologList.getLength(); i++) {
      mapList.add(new MapImpl(prologList.getTermAt(i)));
    }
    return mapList;
  }

  class EvImpl implements Ev {
    Term evTerm;
    public EvImpl(Term newEvTerm) throws Exception {
      if (newEvTerm.isCompound())
	this.evTerm = newEvTerm;
      else
	throw new Exception("supplied term is not a compound term.");
    }
    public int getScore() throws Exception { 
      return TermUtils.getIntegerArgument(this.evTerm, 1);
    }
    public String getConceptId() throws Exception {
      return TermUtils.getAtomArgument(this.evTerm, 2);
    }
    public String getConceptName() throws Exception  {
      return TermUtils.getAtomArgument(this.evTerm, 3);
    }
    public String getPreferredName() throws Exception  { 
      return TermUtils.getAtomArgument(this.evTerm, 4);
    }
    public List<String> getMatchedWords() throws Exception  { 
      return TermUtils.getAtomStringListArgument(this.evTerm, 5);
    }
    public List<String> getSemanticTypes() throws Exception  {
      return TermUtils.getAtomStringListArgument(this.evTerm, 6);
    }
    public boolean isHead() throws Exception  { 
      return TermUtils.getAtomArgument(this.evTerm, 8).equals("yes");
    }
    public boolean isOvermatch() throws Exception  {       
      return TermUtils.getAtomArgument(this.evTerm, 9).equals("yes");
    }
    public List<String> getSources() throws Exception  {
      return TermUtils.getAtomStringListArgument(this.evTerm, 10);
    }
    public List<Position> getPositionalInfo() throws Exception  { 
      return TermUtils.getPositionListArgument(this.evTerm, 11);
    }
  }

  class MapImpl implements Map {
    Term mapTerm;
    public MapImpl(Term newMapTerm) throws Exception {
      if (newMapTerm.isCompound())
	this.mapTerm = newMapTerm;
      else
	throw new Exception("supplied term is not a compound term.");
    }
    public int getScore() throws Exception { 
      return TermUtils.getIntegerArgument(this.mapTerm, 1);
    }
    public List<Ev> getEvList() 
      throws Exception 
    {
      List<Ev> evList = new ArrayList<Ev>();
      PBList prologList = (PBList)this.mapTerm.getArgument(2);
      for (int i = 1; i <= prologList.getLength(); i++) {
	evList.add(new EvImpl(prologList.getTermAt(i)));
      }
      return evList;
    }
  }
}

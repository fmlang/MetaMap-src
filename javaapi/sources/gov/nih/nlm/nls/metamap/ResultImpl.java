package gov.nih.nlm.nls.metamap;

/**
 * Implementation of Result interface.
 *
 *<p>
 * Created: Wed May 20 14:52:47 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */

import java.util.List;
import java.util.ArrayList;
import java.io.PrintStream;
import se.sics.prologbeans.*;
import gov.nih.nlm.nls.metamap.TermUtils;

/**
 * An implemention of the <code>Result</code> interface, a container
 * for the result of a MetaMap query.
 *
 *
 * Created: Wed May 20 13:50:35 2009
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version 1.0
 */
public class ResultImpl implements Result {

  private PBList mmoTermList;
  // private static final int ARGS_INDEX             = 1;
  private static final int ACRONYMS_ABBREVS_INDEX = 2;
  private static final int NEGATIONLIST_INDEX     = 3;

  private static final int FIRST_UTTERANCE_INDEX  = 4;
  // private static final int PHRASE_INDEX           = 5;
  // private static final int CANDIDATES_INDEX       = 6;
  // private static final int MAPPINGS_INDEX         = 7;

  /**
   * Creates a new <code>ResultImpl</code> instance.
   *
   */
  public ResultImpl() {

  }

  /**
   * Creates a new <code>ResultImpl</code> instance.
   * @param mmoTerm a prolog term containing MetaMap machine output.
   */
  public ResultImpl(Term mmoTerm) throws Exception {
    if (mmoTerm.isList()) {
      this.mmoTermList = (PBList)mmoTerm;
    } else {
      throw new Exception("resulting term is not a legal machine output termlist");
    }
  }
  // Implementation of gov.nih.nlm.nls.metamap.Result

  /** @return prolog beans list of machine output terms */
  public PBList getMMOPBlist()
  {
    return this.mmoTermList;
  }

  public void traverse(PrintStream out)
  {
    for (int i = 1; i<this.mmoTermList.getLength(); i++) {
      out.println(this.mmoTermList.getTermAt(i));
    }
  }

  /** @return a <code>String</code> containing the result as MetaMap
   * machine output. */
  public String getMachineOutput()
  {
    StringBuffer sb = new StringBuffer();
    for (int i = 1; i < this.mmoTermList.getLength(); i++) {
      Term listTerm = this.mmoTermList.getTermAt(i);
      sb.append(listTerm.toString()).append("\n");
    }
    return sb.toString();
  }

  /**
   * Describe <code>getAcronymsAbbrevs</code> method here.
   *
   * @return a <code>List</code> of 
   */
  public final List<AcronymsAbbrevs> getAcronymsAbbrevs()  throws Exception {
    List<AcronymsAbbrevs> aasList = new ArrayList<AcronymsAbbrevs>();
    Term listTerm = this.mmoTermList.getTermAt(ACRONYMS_ABBREVS_INDEX);
    PBList prologList = (PBList)listTerm.getArgument(1);
    for (int i = 1; i < prologList.getLength(); i++) {
      aasList.add(new AcronymsAbbrevsImpl(prologList.getArgument(i)));
    }
    return aasList;
  }

  /**
   * Describe <code>getNegations</code> method here.
   *
   * @return a <code>List</code> of Negation Instances.
   */
  public final List<Negation> getNegations() throws Exception {
    List<Negation> negList = new ArrayList<Negation>();
    Term listTerm = this.mmoTermList.getTermAt(NEGATIONLIST_INDEX);
    PBList prologList = (PBList)listTerm.getArgument(1);
    for (int i = 1; i <= prologList.getLength(); i++) {
      negList.add(new NegationImpl(prologList.getTermAt(i)));
    }
    return negList;
  }

  /**
   * Describe <code>getUtterances</code> method here.
   *
   * @return a list of <code>Utterance</code> terms 
   */
  public List<Utterance> getUtteranceList() throws Exception {

    List<Utterance> utteranceList = new ArrayList<Utterance>();
    for (int i = FIRST_UTTERANCE_INDEX; i<=this.mmoTermList.getLength(); i++) {
      if (this.mmoTermList.getTermAt(i).getName().equals("utterance")) {
	utteranceList.add(new UtteranceImpl(this.mmoTermList.getTermAt(i), i, this));
      }
    }
    return utteranceList;
  }

  /** 
   * Return a list of <code>PCM</code> (Phrase/Candidate/Mapping)
   * objects starting at start in machine output.  It is recommended
   * to use the <code>Utterance</code> implementation of getPCMList
   * rather than this one.
   *
   * @param utterancePosition position of relevant utterance
   * @return list of Phrase/Candidates/Mappings (PCM) objects
   */
  public List<PCM> getPCMList(int utterancePosition) throws Exception {
    int start = utterancePosition + 1;
    List<PCM> pcmList = new ArrayList<PCM>();
    for (int i = start; i <= this.mmoTermList.getLength(); i=i+3) {
      if ( this.mmoTermList.getTermAt(i).getName().equals("phrase") ) {
	Term phrase = this.mmoTermList.getTermAt(i);
	Term candidates = null;
	Term mappings = null;
	int j=i+1;
	if ( this.mmoTermList.getTermAt(j).getName().equals("candidates") ) {
	  candidates = this.mmoTermList.getTermAt(j);
	} else {
	  this.mmoTermList.getTermAt(j).getName().equals("utterance");
	  break;
	}
	j++;
	if ( this.mmoTermList.getTermAt(j).getName().equals("mappings") ) {
	  mappings = this.mmoTermList.getTermAt(j);
	}
	pcmList.add(new PCMBase(phrase, candidates, mappings));
      } else {
	this.mmoTermList.getTermAt(i).getName().equals("utterance");
	break;
      }
    }
    return pcmList;
  }

  /**	  
   * <pre>
   * aas(["ABC"*"American Broadcasting System" *[1,3,5,28]*[],
   *      "CBS"*"Columbia Broadcasting System" *[1,3,5,28]*[]]).
   * </pre>
   */
  class AcronymsAbbrevsImpl implements AcronymsAbbrevs {
    Term aasTerm;

    public AcronymsAbbrevsImpl(Term newAATerm) throws Exception {
      if (newAATerm.isCompound())
	aasTerm = newAATerm;
      else
	throw new Exception("supplied term is not a compound term.");
    }
    public String getAcronym() {
      return aasTerm.getArgument(1).getArgument(1).getArgument(1).toString();
    }
    public String getExpansion() {
      return aasTerm.getArgument(1).getArgument(1).getArgument(2).toString();
    };
    public List<Integer> getCountList() {
      List<Integer> countList = new ArrayList<Integer>();
      PBList prologlist = (PBList)aasTerm.getArgument(1).getArgument(2);
      for (int i = 1; i <= prologlist.getLength(); i++) {
	countList.add(new Integer(prologlist.getTermAt(i).intValue()));
      }
      return countList;
    };
    public List<String> getCUIList() {
      List<String> cuiList = new ArrayList<String>();
      /* get the second argument which contains the prolog list
       * containing the cuis */
      PBList prologlist = (PBList)aasTerm.getArgument(2);
      for (int i = 1; i <= prologlist.getLength(); i++) {
	cuiList.add(prologlist.getTermAt(1).toString());
      }
      return cuiList;
    };
  }

  class NegationImpl implements Negation {

    class ConceptPairImpl implements ConceptPair
    {
      String conceptId;
      String preferredName;
      ConceptPairImpl(String theConceptId, String thePreferredName)
      {
	this.conceptId = theConceptId;
	this.preferredName = thePreferredName;
      }
      public String getConceptId() { return conceptId; }
      public String getPreferredName() { return preferredName; }
      public String toString() {
	return "(" + conceptId + "," + preferredName + ")";
      }
    }

    Term negTerm;
    public NegationImpl(Term newNegTerm) throws Exception {
      if (newNegTerm.isCompound())
	negTerm = newNegTerm;
      else
	throw new Exception("supplied term is not a compound term.");
    }
    public String getType() throws Exception { 
      return TermUtils.getAtomArgument(this.negTerm, 1);
    }
    public String getTrigger() throws Exception { 
      return TermUtils.getAtomArgument(this.negTerm, 2);
    } 
    public List<Position> getTriggerPositionList() throws Exception { 
      return TermUtils.getPositionListArgument(this.negTerm, 3);      
    }
    public List<ConceptPair> getConceptPairList() throws Exception {
      List<ConceptPair> pairList = new ArrayList<ConceptPair>();
      PBList plist = ((PBList)this.negTerm.getArgument(4));
      for (int i = 1; i <= plist.getLength(); i++) {
	Term term = plist.getTermAt(i);
	pairList.add
	  (new ConceptPairImpl(term.getArgument(1).toString(),
			       term.getArgument(2).toString()));
      }
      return pairList;
    }
    public List<Position> getConceptPositionList() throws Exception {
      return TermUtils.getPositionListArgument(this.negTerm, 5);      
    }
    public String getConceptId() throws Exception {
       return null;		// not implemented
    }
  }

  class UtteranceImpl implements Utterance {
    Term utteranceTerm;
    int mmoListPosition;
    private Result result;

    /**
     * @param newUtteranceTerm PrologBeans term representation of utterance term.
     * @param listPosition position of term in machine output term list.
     * @param theResult the MetaMap result.
     */
    public UtteranceImpl(Term newUtteranceTerm,
			 int listPosition,
			 Result theResult)
      throws Exception
    {
      if (newUtteranceTerm.isCompound()) {
	this.utteranceTerm = newUtteranceTerm;
	this.mmoListPosition = listPosition;
	this.result = theResult;
      } else {
	throw new Exception("supplied pterm is not a compound term.");
      }
    }
    public String getId() throws Exception { 
      return TermUtils.getAtomArgument(this.utteranceTerm, 1);
    }
    public String getString() throws Exception {
      return TermUtils.getStringArgument(this.utteranceTerm, 2);
    }
    public Position getPosition() throws Exception { 
      return TermUtils.getPositionArgument(this.utteranceTerm, 3);
    }

    /**
     * Return a list of <code>PCM</code> (Phrase/Candidate/Mapping)
     * objects associated with the utterance.
     * @return a list of <code>PCM</code> (Phrase/Candidate/Mapping) objects.
     */
    public List<PCM> getPCMList() throws Exception {
      return this.result.getPCMList(this.getMMOPosition());
    }
    public int getMMOPosition() throws Exception {
      return this.mmoListPosition;
    }
    public String toString() {
      try {
	return "utterance: " + this.getId() + "," + this.getString() + "," +this.getPosition();
      } catch (Exception e) {
	e.printStackTrace();	// TODO: should use RunTimeException
      }
      return "n/a";
    }
  }
}

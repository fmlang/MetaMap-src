static Derror _derror [] =
{
	{  D_S_SUCCESS,         SUCCESS, "D_S_SUCCESS",          "Success"},
	{  D_E_ERROR,           ERROR,   "D_E_ERROR",            "Error"},
	{  D_F_MALLOC,          FATAL,   "D_E_MALLOC",           "Call to malloc failed"},
	{  D_E_FILE,            ERROR,   "D_E_FILE",             "Error trying to open or close a file"},
	{  D_E_FSEEK,           ERROR,   "D_E_FSEEK",            "Call to fseek failed"},
	{  D_E_FTELL,           ERROR,   "D_E_FTELL",            "Call to ftell failed"},
	{ D_E_FPUTS,            ERROR,   "D_E_FPUTS",            "Call to fputs failed"},
	{ D_F_ERROR,            FATAL,   "D_F_ERROR",            "Fatal Error"},
	{ D_S_NUMBER_EXISTS,    SUCCESS, "D_S_NUMBER_EXISTS",    "The Entry is not a new entry"},
	{ D_S_EOF,              SUCCESS, "D_S_EOF",              "End of File Hit"},
	{ D_S_NOT_FOUND,        SUCCESS, "D_S_NOT_FOUND",        "Not found"},
	{ D_E_ERRLOG,           ERROR,   "D_E_ERRLOG",           "Unable to write to the error log"},
	{ D_E_DBGLOG,           ERROR,   "D_E_DBGLOG",           "Unable to write to the debug log"},
	{ D_F_VALNODENEW,       FATAL,   "D_F_VALNODENEW",       "Unable to create a new valnode"},
	{ D_W_UNEXPECTED_VALUE, WARNING, "D_W_UNEXPECTED_VALUE", "Unexpected or Unknown value has been seen by this routine"},
	{ D_E_UNEXPECTED_VALUE, ERROR,   "D_E_UNEXPECTED_VALUE", "An Unexpected or Unknown value has been seen by this routine"},
	{ D_W_DESIGN_DEFECT,    WARNING, "D_W_DESIGN_DEFECT",    "A Design defect"},
	{ D_F_ASNLOAD,          FATAL,   "D_F_ASNLOAD",          "Unable to initialize asn api s"},
	{ D_F_ASNIOOPEN,        FATAL,   "D_F_ASNIOOPEN",        "Unable to open ASN encoded file"}
};


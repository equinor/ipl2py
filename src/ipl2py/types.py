from enum import Enum


class IplTypes(str, Enum):
    BOOL = "Bool"
    INT = "Int"
    FLOAT = "Float"
    STRING = "String"
    GRID_MODEL = "GridModel"
    PARAMETER = "Parameter"
    BODY = "Body"
    POINT = "Point"
    POINTS = "Points"
    FILE = "File"
    JOB = "Job"
    INCLUDE = "Include"
    FUNCTION_X = "FunctionX"
    FUNCTION_XY = "FunctionXY"
    SURFACE = "Surface"
    POLYGONS = "Polygons"
    LOG = "Log"
    BLOCKED_LOG = "BlockedLog"
    DISTRIBUTION_X = "DistributionX"
    DISTRIBUTION_XY = "DistributionXY"
    FACIES_MODEL = "FaciesModel"


class IplConsts(str, Enum):
    E = "E"
    PI = "PI"
    DEG = "DEG"
    RAD = "RAD"

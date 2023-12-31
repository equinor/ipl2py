from dataclasses import dataclass
from enum import Enum
from typing import List


class Type(str, Enum):
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


class Constant(str, Enum):
    E = "E"
    PI = "PI"
    DEG = "DEG"
    RAD = "RAD"


class SysDef(str, Enum):
    GRIDMODELS = "@GRIDMODELS"
    HORIZONS = "@HORIZONS"
    X = "@X"
    Y = "@Y"
    Z = "@Z"
    X1 = "@X1"
    X2 = "@X2"
    X3 = "@X3"
    X4 = "@X4"
    X5 = "@X5"
    X6 = "@X6"
    X7 = "@X7"
    X8 = "@X8"
    Y1 = "@Y1"
    Y2 = "@Y2"
    Y3 = "@Y3"
    Y4 = "@Y4"
    Y5 = "@Y5"
    Y6 = "@Y6"
    Y7 = "@Y7"
    Y8 = "@Y8"
    Z1 = "@Z1"
    Z2 = "@Z2"
    Z3 = "@Z3"
    Z4 = "@Z4"
    Z5 = "@Z5"
    Z6 = "@Z6"
    Z7 = "@Z7"
    Z8 = "@Z8"
    DX = "@dX"
    DY = "@dY"
    DZ = "@dZ"
    I = "@I"  # noqa
    J = "@J"
    K = "@K"
    N_GRIDMODELS = "@N_GRIDMODELS"
    N_WELLS = "@N_WELLS"
    POPDOWN = "@POPDOWN"


@dataclass
class BuiltinFunc:
    ipl_name: str
    py_name: str
    imports: List[str]


BUILTINS = {"Print": BuiltinFunc(ipl_name="Print", py_name="print", imports=[])}

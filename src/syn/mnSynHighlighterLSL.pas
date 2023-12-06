unit mnSynHighlighterLSL;
{$mode objfpc}{$H+}
{**
 * NOT COMPLETED
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *
 * https://github.com/parmaja/miniedit/issues/89
 * https://github.com/Makopo/lslint
 *
 *}

interface

uses
  Classes, SysUtils, Contnrs,
  SynEdit, SynEditTypes,
  SynHighlighterHashEntries, SynEditHighlighter, mnSynHighlighterMultiProc;

type

  { TLSLProcessor }

  TLSLProcessor = class(TCommonSynProcessor)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
    function CreateKeywords: TSynKeywords; override;
  public
    procedure Created; override;
    procedure QuestionProc;
    procedure SlashProc;
    procedure BlockProc;

    procedure GreaterProc;
    procedure LowerProc;
    procedure DeclareProc;

    procedure Next; override;

    procedure Prepare; override;
    procedure MakeProcTable; override;
  end;

  { TSynDSyn }

  TSynLSLSyn = class(TSynMultiProcSyn)
  private
  protected
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitProcessors; override;
  published
  end;

var
  LSLKeywords: TSynKeywords;

const

  SYNS_LangLSL = 'LSL';
  SYNS_FilterLSL = 'LSL Lang Files (*.LSL)|*.LSL';

  cLSLSample =  '/**'
                +'    This examples are worked, and this comment will ignored, not compiled or parsed as we say.'#13
                +'  */'#13
                +'//* Single Line documentation'#13
                +'// Single Line comment'#13
                +'key avatar;'#13
                +'/* integer count;'#13
                +'integer isExist = TRUE; */'#13
                +''#13
                +'default {'#13
                +'  state_entry {'#13
                +'    llSay(0, "Hello World");'#13
                +'  }'#13
                +'  '#13
                +'  collision_start(integer total_number){'#13
                +'    avatar = llDetectedKey(0);'#13
                +'    if (avatar != NULL_KEY)'#13
                +'    {'#13
                +'        llRequestPermissions(avatar, PERMISSION_TELEPORT);'#13
                +'    }'#13
                +'  }'#13
                +''#13
                +'}'#13;

const
  sLSLTypes =
    'integer,'+
    'float,'+
    'string,'+
    'key,'+
    'vector,'+
    'rotation,'+
    'quaternion,'+
    'list';

  sLSLKeywords =
    'default,'+
    'state,'+
    'event,'+
    'jump,'+
    'return,'+
    'if,'+
    'else,'+
    'for,'+
    'do,'+
    'while,'+
    'print';

  sLSLValues =
    'TRUE,'+
    'FALSE,'+
    'STATUS_PHYSICS,'+
    'STATUS_PHANTOM,'+
    'STATUS_ROTATE_X,'+
    'STATUS_ROTATE_Y,'+
    'STATUS_ROTATE_Z,'+
    'STATUS_SANDBOX,'+
    'STATUS_BLOCK_GRAB,'+
    'STATUS_BLOCK_GRAB_OBJECT,'+
    'STATUS_DIE_AT_EDGE,'+
    'STATUS_RETURN_AT_EDGE,'+
    'STATUS_CAST_SHADOWS,'+
    'AGENT,'+
    'AGENT_BY_USERNAME,'+
    'AGENT_BY_LEGACY_NAME,'+
    'ACTIVE,'+
    'PASSIVE,'+
    'SCRIPTED,'+
    'CONTROL_FWD,'+
    'CONTROL_BACK,'+
    'CONTROL_LEFT,'+
    'CONTROL_RIGHT,'+
    'CONTROL_ROT_LEFT,'+
    'CONTROL_ROT_RIGHT,'+
    'CONTROL_UP,'+
    'CONTROL_DOWN,'+
    'CONTROL_LBUTTON,'+
    'CONTROL_ML_LBUTTON,'+
    'PERMISSION_DEBIT,'+
    'PERMISSION_TAKE_CONTROLS,'+
    'PERMISSION_REMAP_CONTROLS,'+
    'PERMISSION_TRIGGER_ANIMATION,'+
    'PERMISSION_ATTACH,'+
    'PERMISSION_RELEASE_OWNERSHIP,'+
    'PERMISSION_CHANGE_LINKS,'+
    'PERMISSION_CHANGE_JOINTS,'+
    'PERMISSION_CHANGE_PERMISSIONS,'+
    'PERMISSION_TRACK_CAMERA,'+
    'PERMISSION_CONTROL_CAMERA,'+
    'PERMISSION_TELEPORT,'+
    'PERMISSION_SILENT_ESTATE_MANAGEMENT,'+
    'PERMISSION_OVERRIDE_ANIMATIONS,'+
    'PERMISSION_RETURN_OBJECTS,'+
    'DEBUG_CHANNEL,'+
    'PUBLIC_CHANNEL,'+
    'AGENT_FLYING,'+
    'AGENT_ATTACHMENTS,'+
    'AGENT_SCRIPTED,'+
    'AGENT_SITTING,'+
    'AGENT_ON_OBJECT,'+
    'AGENT_MOUSELOOK,'+
    'AGENT_AWAY,'+
    'AGENT_WALKING,'+
    'AGENT_IN_AIR,'+
    'AGENT_TYPING,'+
    'AGENT_CROUCHING,'+
    'AGENT_BUSY,'+
    'AGENT_ALWAYS_RUN,'+
    'AGENT_AUTOPILOT,'+
    'AGENT_LIST_PARCEL,'+
    'AGENT_LIST_PARCEL_OWNER,'+
    'AGENT_LIST_REGION,'+
    'PSYS_PART_FLAGS,'+
    'PSYS_PART_START_COLOR,'+
    'PSYS_PART_START_ALPHA,'+
    'PSYS_PART_START_SCALE,'+
    'PSYS_PART_END_COLOR,'+
    'PSYS_PART_END_ALPHA,'+
    'PSYS_PART_END_SCALE,'+
    'PSYS_PART_MAX_AGE,'+
    'PSYS_PART_INTERP_COLOR_MASK,'+
    'PSYS_PART_INTERP_SCALE_MASK,'+
    'PSYS_PART_BOUNCE_MASK,'+
    'PSYS_PART_WIND_MASK,'+
    'PSYS_PART_FOLLOW_SRC_MASK,'+
    'PSYS_PART_FOLLOW_VELOCITY_MASK,'+
    'PSYS_PART_TARGET_POS_MASK,'+
    'PSYS_PART_TARGET_LINEAR_MASK,'+
    'PSYS_PART_EMISSIVE_MASK,'+
    'PSYS_PART_RIBBON_MASK,'+
    'PSYS_SRC_PATTERN,'+
    'PSYS_PART_BF_ONE,'+
    'PSYS_PART_BF_ZERO,'+
    'PSYS_PART_BF_DEST_COLOR,'+
    'PSYS_PART_BF_SOURCE_COLOR,'+
    'PSYS_PART_BF_ONE_MINUS_DEST_COLOR,'+
    'PSYS_PART_BF_ONE_MINUS_SOURCE_COLOR,'+
    'PSYS_PART_BF_SOURCE_ALPHA,'+
    'PSYS_PART_BF_ONE_MINUS_SOURCE_ALPHA,'+
    'PSYS_SRC_INNERANGLE,'+
    'PSYS_SRC_OUTERANGLE,'+
    'PSYS_SRC_ANGLE_BEGIN,'+
    'PSYS_SRC_ANGLE_END,'+
    'PSYS_SRC_BURST_RATE,'+
    'PSYS_SRC_BURST_PART_COUNT,'+
    'PSYS_SRC_BURST_RADIUS,'+
    'PSYS_SRC_BURST_SPEED_MIN,'+
    'PSYS_SRC_BURST_SPEED_MAX,'+
    'PSYS_SRC_MAX_AGE,'+
    'PSYS_SRC_ACCEL,'+
    'PSYS_SRC_TEXTURE,'+
    'PSYS_SRC_TARGET_KEY,'+
    'PSYS_SRC_OMEGA,'+
    'PSYS_SRC_PATTERN_DROP,'+
    'PSYS_SRC_PATTERN_EXPLODE,'+
    'PSYS_SRC_PATTERN_ANGLE,'+
    'PSYS_SRC_PATTERN_ANGLE_CONE,'+
    'PSYS_SRC_PATTERN_ANGLE_CONE_EMPTY,'+
    'PSYS_SRC_OBJ_REL_MASK,'+
    'PSYS_PART_BLEND_FUNC_SOURCE,'+
    'PSYS_PART_BLEND_FUNC_DEST,'+
    'PSYS_PART_START_GLOW,'+
    'PSYS_PART_END_GLOW,'+
    'OBJECT_UNKNOWN_DETAIL,'+
    'OBJECT_NAME,'+
    'OBJECT_DESC,'+
    'OBJECT_POS,'+
    'OBJECT_ROT,'+
    'OBJECT_VELOCITY,'+
    'OBJECT_OWNER,'+
    'OBJECT_GROUP,'+
    'OBJECT_CREATOR,'+
    'OBJECT_RUNNING_SCRIPT_COUNT,'+
    'OBJECT_TOTAL_SCRIPT_COUNT,'+
    'OBJECT_SCRIPT_MEMORY,'+
    'OBJECT_SCRIPT_TIME,'+
    'OBJECT_PRIM_EQUIVALENCE,'+
    'OBJECT_PHYSICS_COST,'+
    'OBJECT_SERVER_COST,'+
    'OBJECT_STREAMING_COST,'+
    'OBJECT_CHARACTER_TIME,'+
    'OBJECT_ROOT,'+
    'OBJECT_ATTACHED_POINT,'+
    'OBJECT_PATHFINDING_TYPE,'+
    'OBJECT_RENDER_WEIGHT,'+
    'OBJECT_HOVER_HEIGHT,'+
    'OBJECT_BODY_SHAPE_TYPE,'+
    'OBJECT_LAST_OWNER_ID,'+
    'OBJECT_CLICK_ACTION,'+
    'OBJECT_OMEGA,'+
    'OPT_AVATAR,'+
    'OPT_CHARACTER,'+
    'OPT_EXCLUSION_VOLUME,'+
    'OPT_LEGACY_LINKSET,'+
    'OPT_MATERIAL_VOLUME,'+
    'OPT_OTHER,'+
    'OPT_STATIC_OBSTACLE,'+
    'OPT_WALKABLE,'+
    'VEHICLE_TYPE_NONE,'+
    'VEHICLE_TYPE_SLED,'+
    'VEHICLE_TYPE_CAR,'+
    'VEHICLE_TYPE_BOAT,'+
    'VEHICLE_TYPE_AIRPLANE,'+
    'VEHICLE_TYPE_BALLOON,'+
    'VEHICLE_REFERENCE_FRAME,'+
    'VEHICLE_LINEAR_FRICTION_TIMESCALE,'+
    'VEHICLE_ANGULAR_FRICTION_TIMESCALE,'+
    'VEHICLE_LINEAR_MOTOR_DIRECTION,'+
    'VEHICLE_LINEAR_MOTOR_OFFSET,'+
    'VEHICLE_ANGULAR_MOTOR_DIRECTION,'+
    'VEHICLE_HOVER_HEIGHT,'+
    'VEHICLE_HOVER_EFFICIENCY,'+
    'VEHICLE_HOVER_TIMESCALE,'+
    'VEHICLE_BUOYANCY,'+
    'VEHICLE_LINEAR_DEFLECTION_EFFICIENCY,'+
    'VEHICLE_LINEAR_DEFLECTION_TIMESCALE,'+
    'VEHICLE_LINEAR_MOTOR_TIMESCALE,'+
    'VEHICLE_LINEAR_MOTOR_DECAY_TIMESCALE,'+
    'VEHICLE_ANGULAR_DEFLECTION_EFFICIENCY,'+
    'VEHICLE_ANGULAR_DEFLECTION_TIMESCALE,'+
    'VEHICLE_ANGULAR_MOTOR_TIMESCALE,'+
    'VEHICLE_ANGULAR_MOTOR_DECAY_TIMESCALE,'+
    'VEHICLE_VERTICAL_ATTRACTION_EFFICIENCY,'+
    'VEHICLE_VERTICAL_ATTRACTION_TIMESCALE,'+
    'VEHICLE_BANKING_EFFICIENCY,'+
    'VEHICLE_BANKING_MIX,'+
    'VEHICLE_BANKING_TIMESCALE,'+
    'VEHICLE_FLAG_NO_FLY_UP,'+
    'VEHICLE_FLAG_NO_DEFLECTION_UP,'+
    'VEHICLE_FLAG_LIMIT_ROLL_ONLY,'+
    'VEHICLE_FLAG_HOVER_WATER_ONLY,'+
    'VEHICLE_FLAG_HOVER_TERRAIN_ONLY,'+
    'VEHICLE_FLAG_HOVER_GLOBAL_HEIGHT,'+
    'VEHICLE_FLAG_HOVER_UP_ONLY,'+
    'VEHICLE_FLAG_LIMIT_MOTOR_UP,'+
    'VEHICLE_FLAG_MOUSELOOK_STEER,'+
    'VEHICLE_FLAG_MOUSELOOK_BANK,'+
    'VEHICLE_FLAG_CAMERA_DECOUPLED,'+
    'CAMERA_PITCH,'+
    'CAMERA_FOCUS_OFFSET,'+
    'CAMERA_POSITION_LAG,'+
    'CAMERA_FOCUS_LAG,'+
    'CAMERA_DISTANCE,'+
    'CAMERA_BEHINDNESS_ANGLE,'+
    'CAMERA_BEHINDNESS_LAG,'+
    'CAMERA_POSITION_THRESHOLD,'+
    'CAMERA_FOCUS_THRESHOLD,'+
    'CAMERA_ACTIVE,'+
    'CAMERA_POSITION,'+
    'CAMERA_FOCUS,'+
    'CAMERA_POSITION_LOCKED,'+
    'CAMERA_FOCUS_LOCKED,'+
    'INVENTORY_TEXTURE,'+
    'INVENTORY_SOUND,'+
    'INVENTORY_OBJECT,'+
    'INVENTORY_SCRIPT,'+
    'INVENTORY_LANDMARK,'+
    'INVENTORY_CLOTHING,'+
    'INVENTORY_NOTECARD,'+
    'INVENTORY_BODYPART,'+
    'INVENTORY_ANIMATION,'+
    'INVENTORY_GESTURE,'+
    'INVENTORY_ALL,'+
    'INVENTORY_NONE,'+
    'ATTACH_CHEST,'+
    'ATTACH_HEAD,'+
    'ATTACH_LSHOULDER,'+
    'ATTACH_RSHOULDER,'+
    'ATTACH_LHAND,'+
    'ATTACH_RHAND,'+
    'ATTACH_LFOOT,'+
    'ATTACH_RFOOT,'+
    'ATTACH_BACK,'+
    'ATTACH_PELVIS,'+
    'ATTACH_MOUTH,'+
    'ATTACH_CHIN,'+
    'ATTACH_LEAR,'+
    'ATTACH_REAR,'+
    'ATTACH_LEYE,'+
    'ATTACH_REYE,'+
    'ATTACH_NOSE,'+
    'ATTACH_RUARM,'+
    'ATTACH_RLARM,'+
    'ATTACH_LUARM,'+
    'ATTACH_LLARM,'+
    'ATTACH_RHIP,'+
    'ATTACH_RULEG,'+
    'ATTACH_RLLEG,'+
    'ATTACH_LHIP,'+
    'ATTACH_LULEG,'+
    'ATTACH_LLLEG,'+
    'ATTACH_BELLY,'+
    'ATTACH_RPEC,'+
    'ATTACH_LPEC,'+
    'ATTACH_LEFT_PEC,'+
    'ATTACH_RIGHT_PEC,'+
    'ATTACH_HUD_BOTTOM,'+
    'ATTACH_HUD_BOTTOM_LEFT,'+
    'ATTACH_HUD_BOTTOM_RIGHT,'+
    'ATTACH_HUD_CENTER_1,'+
    'ATTACH_HUD_CENTER_2,'+
    'ATTACH_HUD_TOP_CENTER,'+
    'ATTACH_HUD_TOP_LEFT,'+
    'ATTACH_HUD_TOP_RIGHT,'+
    'ATTACH_NECK,'+
    'ATTACH_AVATAR_CENTER,'+
    'ATTACH_LHAND_RING1,'+
    'ATTACH_RHAND_RING1,'+
    'ATTACH_TAIL_BASE,'+
    'ATTACH_TAIL_TIP,'+
    'ATTACH_LWING,'+
    'ATTACH_RWING,'+
    'ATTACH_FACE_JAW,'+
    'ATTACH_FACE_LEAR,'+
    'ATTACH_FACE_REAR,'+
    'ATTACH_FACE_LEYE,'+
    'ATTACH_FACE_REYE,'+
    'ATTACH_FACE_TONGUE,'+
    'ATTACH_GROIN,'+
    'ATTACH_HIND_LFOOT,'+
    'ATTACH_HIND_RFOOT,'+
    'LAND_LEVEL,'+
    'LAND_RAISE,'+
    'LAND_LOWER,'+
    'LAND_SMOOTH,'+
    'LAND_NOISE,'+
    'LAND_REVERT,'+
    'LAND_SMALL_BRUSH,'+
    'LAND_MEDIUM_BRUSH,'+
    'LAND_LARGE_BRUSH,'+
    'DATA_PAYINFO,'+
    'DATA_ONLINE,'+
    'DATA_NAME,'+
    'DATA_BORN,'+
    'DATA_RATING,'+
    'DATA_SIM_POS,'+
    'DATA_SIM_STATUS,'+
    'DATA_SIM_RATING,'+
    'PAYMENT_INFO_ON_FILE,'+
    'PAYMENT_INFO_USED,'+
    'ANIM_ON,'+
    'LOOP,'+
    'REVERSE,'+
    'PING_PONG,'+
    'SMOOTH,'+
    'ROTATE,'+
    'SCALE,'+
    'ALL_SIDES,'+
    'LINK_SET,'+
    'LINK_ROOT,'+
    'LINK_ALL_OTHERS,'+
    'LINK_ALL_CHILDREN,'+
    'LINK_THIS,'+
    'CHANGED_INVENTORY,'+
    'CHANGED_COLOR,'+
    'CHANGED_SHAPE,'+
    'CHANGED_SCALE,'+
    'CHANGED_TEXTURE,'+
    'CHANGED_LINK,'+
    'CHANGED_ALLOWED_DROP,'+
    'CHANGED_OWNER,'+
    'CHANGED_REGION,'+
    'CHANGED_TELEPORT,'+
    'CHANGED_REGION_START,'+
    'CHANGED_MEDIA,'+
    'TYPE_INTEGER,'+
    'TYPE_FLOAT,'+
    'TYPE_STRING,'+
    'TYPE_KEY,'+
    'TYPE_VECTOR,'+
    'TYPE_ROTATION,'+
    'TYPE_INVALID,'+
    'REMOTE_DATA_CHANNEL,'+
    'REMOTE_DATA_REQUEST,'+
    'REMOTE_DATA_REPLY,'+
    'PRIM_TYPE,'+
    'PRIM_MATERIAL,'+
    'PRIM_PHYSICS,'+
    'PRIM_FLEXIBLE,'+
    'PRIM_POINT_LIGHT,'+
    'PRIM_TEMP_ON_REZ,'+
    'PRIM_PHANTOM,'+
    'PRIM_CAST_SHADOWS,'+
    'PRIM_POSITION,'+
    'PRIM_POS_LOCAL,'+
    'PRIM_SIZE,'+
    'PRIM_ROTATION,'+
    'PRIM_ROT_LOCAL,'+
    'PRIM_TEXTURE,'+
    'PRIM_COLOR,'+
    'PRIM_BUMP_SHINY,'+
    'PRIM_FULLBRIGHT,'+
    'PRIM_TEXGEN,'+
    'PRIM_GLOW,'+
    'PRIM_TEXT,'+
    'PRIM_NAME,'+
    'PRIM_DESC,'+
    'PRIM_OMEGA,'+
    'PRIM_LINK_TARGET,'+
    'PRIM_PHYSICS_SHAPE_TYPE,'+
    'PRIM_SLICE,'+
    'PRIM_TYPE_BOX,'+
    'PRIM_TYPE_CYLINDER,'+
    'PRIM_TYPE_PRISM,'+
    'PRIM_TYPE_SPHERE,'+
    'PRIM_TYPE_TORUS,'+
    'PRIM_TYPE_TUBE,'+
    'PRIM_TYPE_RING,'+
    'PRIM_TYPE_SCULPT,'+
    'PRIM_HOLE_DEFAULT,'+
    'PRIM_HOLE_SQUARE,'+
    'PRIM_HOLE_CIRCLE,'+
    'PRIM_HOLE_TRIANGLE,'+
    'PRIM_MATERIAL_STONE,'+
    'PRIM_MATERIAL_METAL,'+
    'PRIM_MATERIAL_GLASS,'+
    'PRIM_MATERIAL_WOOD,'+
    'PRIM_MATERIAL_FLESH,'+
    'PRIM_MATERIAL_PLASTIC,'+
    'PRIM_MATERIAL_RUBBER,'+
    'PRIM_MATERIAL_LIGHT,'+
    'PRIM_SHINY_NONE,'+
    'PRIM_SHINY_LOW,'+
    'PRIM_SHINY_MEDIUM,'+
    'PRIM_SHINY_HIGH,'+
    'PRIM_BUMP_NONE,'+
    'PRIM_BUMP_BRIGHT,'+
    'PRIM_BUMP_DARK,'+
    'PRIM_BUMP_WOOD,'+
    'PRIM_BUMP_BARK,'+
    'PRIM_BUMP_BRICKS,'+
    'PRIM_BUMP_CHECKER,'+
    'PRIM_BUMP_CONCRETE,'+
    'PRIM_BUMP_TILE,'+
    'PRIM_BUMP_STONE,'+
    'PRIM_BUMP_DISKS,'+
    'PRIM_BUMP_GRAVEL,'+
    'PRIM_BUMP_BLOBS,'+
    'PRIM_BUMP_SIDING,'+
    'PRIM_BUMP_LARGETILE,'+
    'PRIM_BUMP_STUCCO,'+
    'PRIM_BUMP_SUCTION,'+
    'PRIM_BUMP_WEAVE,'+
    'PRIM_TEXGEN_DEFAULT,'+
    'PRIM_TEXGEN_PLANAR,'+
    'PRIM_SCULPT_TYPE_SPHERE,'+
    'PRIM_SCULPT_TYPE_TORUS,'+
    'PRIM_SCULPT_TYPE_PLANE,'+
    'PRIM_SCULPT_TYPE_CYLINDER,'+
    'PRIM_SCULPT_TYPE_MASK,'+
    'PRIM_SCULPT_FLAG_INVERT,'+
    'PRIM_SCULPT_FLAG_MIRROR,'+
    'PRIM_PHYSICS_SHAPE_PRIM,'+
    'PRIM_PHYSICS_SHAPE_CONVEX,'+
    'PRIM_PHYSICS_SHAPE_NONE,'+
    'DENSITY,'+
    'FRICTION,'+
    'RESTITUTION,'+
    'GRAVITY_MULTIPLIER,'+
    'MASK_BASE,'+
    'MASK_OWNER,'+
    'MASK_GROUP,'+
    'MASK_EVERYONE,'+
    'MASK_NEXT,'+
    'PERM_TRANSFER,'+
    'PERM_MODIFY,'+
    'PERM_COPY,'+
    'PERM_MOVE,'+
    'PERM_ALL,'+
    'PARCEL_MEDIA_COMMAND_STOP,'+
    'PARCEL_MEDIA_COMMAND_PAUSE,'+
    'PARCEL_MEDIA_COMMAND_PLAY,'+
    'PARCEL_MEDIA_COMMAND_LOOP,'+
    'PARCEL_MEDIA_COMMAND_LOOP_SET,'+
    'PARCEL_MEDIA_COMMAND_TEXTURE,'+
    'PARCEL_MEDIA_COMMAND_URL,'+
    'PARCEL_MEDIA_COMMAND_TYPE,'+
    'PARCEL_MEDIA_COMMAND_DESC,'+
    'PARCEL_MEDIA_COMMAND_TIME,'+
    'PARCEL_MEDIA_COMMAND_SIZE,'+
    'PARCEL_MEDIA_COMMAND_AGENT,'+
    'PARCEL_MEDIA_COMMAND_UNLOAD,'+
    'PARCEL_MEDIA_COMMAND_AUTO_ALIGN,'+
    'PAY_HIDE,'+
    'PAY_DEFAULT,'+
    'LIST_STAT_MAX,'+
    'LIST_STAT_MIN,'+
    'LIST_STAT_MEAN,'+
    'LIST_STAT_MEDIAN,'+
    'LIST_STAT_STD_DEV,'+
    'LIST_STAT_SUM,'+
    'LIST_STAT_SUM_SQUARES,'+
    'LIST_STAT_NUM_COUNT,'+
    'LIST_STAT_GEOMETRIC_MEAN,'+
    'LIST_STAT_RANGE,'+
    'PARCEL_FLAG_ALLOW_FLY,'+
    'PARCEL_FLAG_ALLOW_GROUP_SCRIPTS,'+
    'PARCEL_FLAG_ALLOW_SCRIPTS,'+
    'PARCEL_FLAG_ALLOW_LANDMARK,'+
    'PARCEL_FLAG_ALLOW_TERRAFORM,'+
    'PARCEL_FLAG_ALLOW_DAMAGE,'+
    'PARCEL_FLAG_ALLOW_CREATE_OBJECTS,'+
    'PARCEL_FLAG_ALLOW_CREATE_GROUP_OBJECTS,'+
    'PARCEL_FLAG_USE_ACCESS_GROUP,'+
    'PARCEL_FLAG_USE_ACCESS_LIST,'+
    'PARCEL_FLAG_USE_BAN_LIST,'+
    'PARCEL_FLAG_USE_LAND_PASS_LIST,'+
    'PARCEL_FLAG_LOCAL_SOUND_ONLY,'+
    'PARCEL_FLAG_RESTRICT_PUSHOBJECT,'+
    'PARCEL_FLAG_ALLOW_ALL_OBJECT_ENTRY,'+
    'PARCEL_FLAG_ALLOW_GROUP_OBJECT_ENTRY,'+
    'REGION_FLAG_ALLOW_DAMAGE,'+
    'REGION_FLAG_FIXED_SUN,'+
    'REGION_FLAG_BLOCK_TERRAFORM,'+
    'REGION_FLAG_SANDBOX,'+
    'REGION_FLAG_DISABLE_COLLISIONS,'+
    'REGION_FLAG_DISABLE_PHYSICS,'+
    'REGION_FLAG_BLOCK_FLY,'+
    'REGION_FLAG_ALLOW_DIRECT_TELEPORT,'+
    'REGION_FLAG_RESTRICT_PUSHOBJECT,'+
    'REGION_FLAG_BLOCK_FLYOVER,'+
    'HTTP_METHOD,'+
    'HTTP_MIMETYPE,'+
    'HTTP_BODY_MAXLENGTH,'+
    'HTTP_VERIFY_CERT,'+
    'HTTP_BODY_TRUNCATED,'+
    'HTTP_CUSTOM_HEADER,'+
    'HTTP_VERBOSE_THROTTLE,'+
    'PARCEL_COUNT_TOTAL,'+
    'PARCEL_COUNT_OWNER,'+
    'PARCEL_COUNT_GROUP,'+
    'PARCEL_COUNT_OTHER,'+
    'PARCEL_COUNT_SELECTED,'+
    'PARCEL_COUNT_TEMP,'+
    'PARCEL_DETAILS_NAME,'+
    'PARCEL_DETAILS_DESC,'+
    'PARCEL_DETAILS_OWNER,'+
    'PARCEL_DETAILS_GROUP,'+
    'PARCEL_DETAILS_AREA,'+
    'PARCEL_DETAILS_ID,'+
    'PARCEL_DETAILS_SEE_AVATARS,'+
    'STRING_TRIM_HEAD,'+
    'STRING_TRIM_TAIL,'+
    'STRING_TRIM,'+
    'CLICK_ACTION_NONE,'+
    'CLICK_ACTION_TOUCH,'+
    'CLICK_ACTION_SIT,'+
    'CLICK_ACTION_BUY,'+
    'CLICK_ACTION_PAY,'+
    'CLICK_ACTION_OPEN,'+
    'CLICK_ACTION_PLAY,'+
    'CLICK_ACTION_OPEN_MEDIA,'+
    'CLICK_ACTION_ZOOM,'+
    'TOUCH_INVALID_FACE,'+
    'PRIM_MEDIA_ALT_IMAGE_ENABLE,'+
    'PRIM_MEDIA_CONTROLS,'+
    'PRIM_MEDIA_CURRENT_URL,'+
    'PRIM_MEDIA_HOME_URL,'+
    'PRIM_MEDIA_AUTO_LOOP,'+
    'PRIM_MEDIA_AUTO_PLAY,'+
    'PRIM_MEDIA_AUTO_SCALE,'+
    'PRIM_MEDIA_AUTO_ZOOM,'+
    'PRIM_MEDIA_FIRST_CLICK_INTERACT,'+
    'PRIM_MEDIA_WIDTH_PIXELS,'+
    'PRIM_MEDIA_HEIGHT_PIXELS,'+
    'PRIM_MEDIA_WHITELIST_ENABLE,'+
    'PRIM_MEDIA_WHITELIST,'+
    'PRIM_MEDIA_PERMS_INTERACT,'+
    'PRIM_MEDIA_PERMS_CONTROL,'+
    'PRIM_MEDIA_PARAM_MAX,'+
    'PRIM_MEDIA_CONTROLS_STANDARD,'+
    'PRIM_MEDIA_CONTROLS_MINI,'+
    'PRIM_MEDIA_PERM_NONE,'+
    'PRIM_MEDIA_PERM_OWNER,'+
    'PRIM_MEDIA_PERM_GROUP,'+
    'PRIM_MEDIA_PERM_ANYONE,'+
    'PRIM_MEDIA_MAX_URL_LENGTH,'+
    'PRIM_MEDIA_MAX_WHITELIST_SIZE,'+
    'PRIM_MEDIA_MAX_WHITELIST_COUNT,'+
    'PRIM_MEDIA_MAX_WIDTH_PIXELS,'+
    'PRIM_MEDIA_MAX_HEIGHT_PIXELS,'+
    'STATUS_OK,'+
    'STATUS_MALFORMED_PARAMS,'+
    'STATUS_TYPE_MISMATCH,'+
    'STATUS_BOUNDS_ERROR,'+
    'STATUS_NOT_FOUND,'+
    'STATUS_NOT_SUPPORTED,'+
    'STATUS_INTERNAL_ERROR,'+
    'STATUS_WHITELIST_FAILED,'+
    'CONTENT_TYPE_TEXT,'+
    'CONTENT_TYPE_HTML,'+
    'CONTENT_TYPE_XML,'+
    'CONTENT_TYPE_XHTML,'+
    'CONTENT_TYPE_ATOM,'+
    'CONTENT_TYPE_JSON,'+
    'CONTENT_TYPE_LLSD,'+
    'CONTENT_TYPE_FORM,'+
    'CONTENT_TYPE_RSS,'+
    'KFM_COMMAND,'+
    'KFM_MODE,'+
    'KFM_DATA,'+
    'KFM_FORWARD,'+
    'KFM_LOOP,'+
    'KFM_PING_PONG,'+
    'KFM_REVERSE,'+
    'KFM_ROTATION,'+
    'KFM_TRANSLATION,'+
    'KFM_CMD_PLAY,'+
    'KFM_CMD_STOP,'+
    'KFM_CMD_PAUSE,'+
    'ESTATE_ACCESS_ALLOWED_AGENT_ADD,'+
    'ESTATE_ACCESS_ALLOWED_AGENT_REMOVE,'+
    'ESTATE_ACCESS_ALLOWED_GROUP_ADD,'+
    'ESTATE_ACCESS_ALLOWED_GROUP_REMOVE,'+
    'ESTATE_ACCESS_BANNED_AGENT_ADD,'+
    'ESTATE_ACCESS_BANNED_AGENT_REMOVE,'+
    'PROFILE_NONE,'+
    'PROFILE_SCRIPT_MEMORY,'+
    'RCERR_UNKNOWN,'+
    'RCERR_SIM_PERF_LOW,'+
    'RCERR_CAST_TIME_EXCEEDED,'+
    'RC_REJECT_TYPES,'+
    'RC_DETECT_PHANTOM,'+
    'RC_DATA_FLAGS,'+
    'RC_MAX_HITS,'+
    'RC_REJECT_AGENTS,'+
    'RC_REJECT_PHYSICAL,'+
    'RC_REJECT_NONPHYSICAL,'+
    'RC_REJECT_LAND,'+
    'RC_GET_NORMAL,'+
    'RC_GET_ROOT_KEY,'+
    'RC_GET_LINK_NUM,'+
    'REQUIRE_LINE_OF_SIGHT,'+
    'PURSUIT_FUZZ_FACTOR,'+
    'PURSUIT_INTERCEPT,'+
    'PURSUIT_GOAL_TOLERANCE,'+
    'PURSUIT_OFFSET,'+
    'FORCE_DIRECT_PATH,'+
    'AVOID_CHARACTERS,'+
    'AVOID_DYNAMIC_OBSTACLES,'+
    'AVOID_NONE,'+
    'PU_EVADE_HIDDEN,'+
    'PU_EVADE_SPOTTED,'+
    'PU_FAILURE_INVALID_GOAL,'+
    'PU_FAILURE_INVALID_START,'+
    'PU_FAILURE_NO_VALID_DESTINATION,'+
    'PU_FAILURE_OTHER,'+
    'PU_FAILURE_TARGET_GONE,'+
    'PU_FAILURE_UNREACHABLE,'+
    'PU_GOAL_REACHED,'+
    'PU_SLOWDOWN_DISTANCE_REACHED,'+
    'PU_FAILURE_NO_NAVMESH,'+
    'PU_FAILURE_DYNAMIC_PATHFINDING_DISABLED,'+
    'PU_FAILURE_PARCEL_UNREACHABLE,'+
    'TRAVERSAL_TYPE,'+
    'TRAVERSAL_TYPE_SLOW,'+
    'TRAVERSAL_TYPE_FAST,'+
    'TRAVERSAL_TYPE_NONE,'+
    'CHARACTER_AVOIDANCE_MODE,'+
    'CHARACTER_CMD_JUMP,'+
    'CHARACTER_CMD_SMOOTH_STOP,'+
    'CHARACTER_CMD_STOP,'+
    'CHARACTER_DESIRED_SPEED,'+
    'CHARACTER_DESIRED_TURN_SPEED,'+
    'CHARACTER_LENGTH,'+
    'CHARACTER_MAX_ACCEL,'+
    'CHARACTER_MAX_DECEL,'+
    'CHARACTER_MAX_SPEED,'+
    'CHARACTER_MAX_TURN_RADIUS,'+
    'CHARACTER_ORIENTATION,'+
    'CHARACTER_RADIUS,'+
    'CHARACTER_TYPE,'+
    'CHARACTER_TYPE_A,'+
    'CHARACTER_TYPE_B,'+
    'CHARACTER_TYPE_C,'+
    'CHARACTER_TYPE_D,'+
    'CHARACTER_TYPE_NONE,'+
    'GCNP_RADIUS,'+
    'GCNP_STATIC,'+
    'HORIZONTAL,'+
    'VERTICAL,'+
    'PATROL_PAUSE_AT_WAYPOINTS,'+
    'WANDER_PAUSE_AT_WAYPOINTS,'+
    'CHARACTER_ACCOUNT_FOR_SKIPPED_FRAMES,'+
    'CHARACTER_STAY_WITHIN_PARCEL,'+
    'SIM_STAT_PCT_CHARS_STEPPED,'+
    'HTTP_PRAGMA_NO_CACHE,'+
    'OBJECT_PHYSICS,'+
    'OBJECT_PHANTOM,'+
    'OBJECT_TEMP_ON_REZ,'+
    'JSON_APPEND,'+
    'OBJECT_RETURN_PARCEL,'+
    'OBJECT_RETURN_PARCEL_OWNER,'+
    'OBJECT_RETURN_REGION,'+
    'ERR_GENERIC,'+
    'ERR_PARCEL_PERMISSIONS,'+
    'ERR_MALFORMED_PARAMS,'+
    'ERR_RUNTIME_PERMISSIONS,'+
    'ERR_THROTTLED,'+
    'PRIM_NORMAL,'+
    'PRIM_SPECULAR,'+
    'PRIM_ALPHA_MODE,'+
    'PRIM_ALPHA_MODE_NONE,'+
    'PRIM_ALPHA_MODE_BLEND,'+
    'PRIM_ALPHA_MODE_MASK,'+
    'PRIM_ALPHA_MODE_EMISSIVE,'+
    'XP_ERROR_NONE,'+
    'XP_ERROR_THROTTLED,'+
    'XP_ERROR_EXPERIENCES_DISABLED,'+
    'XP_ERROR_INVALID_PARAMETERS,'+
    'XP_ERROR_NOT_PERMITTED,'+
    'XP_ERROR_NO_EXPERIENCE,'+
    'XP_ERROR_NOT_FOUND,'+
    'XP_ERROR_INVALID_EXPERIENCE,'+
    'XP_ERROR_EXPERIENCE_DISABLED,'+
    'XP_ERROR_EXPERIENCE_SUSPENDED,'+
    'XP_ERROR_UNKNOWN_ERROR,'+
    'XP_ERROR_QUOTA_EXCEEDED,'+
    'XP_ERROR_STORE_DISABLED,'+
    'XP_ERROR_STORAGE_EXCEPTION,'+
    'XP_ERROR_KEY_NOT_FOUND,'+
    'XP_ERROR_RETRY_UPDATE,'+
    'XP_ERROR_MATURITY_EXCEEDED,'+
    'XP_ERROR_NOT_PERMITTED_LAND,'+
    'XP_ERROR_REQUEST_PERM_TIMEOUT,'+
    'PASS_IF_NOT_HANDLED,'+
    'PASS_ALWAYS,'+
    'PASS_NEVER,'+
    'OBJECT_PRIM_COUNT,'+
    'OBJECT_TOTAL_INVENTORY_COUNT,'+
    'OBJECT_REZZER_KEY,'+
    'PRIM_ALLOW_UNSIT,'+
    'PRIM_SCRIPTED_SIT_ONLY,'+
    'PRIM_SIT_TARGET,'+
    'OBJECT_GROUP_TAG,'+
    'OBJECT_TEMP_ATTACHED,'+
    'OBJECT_ATTACHED_SLOTS_AVAILABLE,'+
    'SIT_NOT_EXPERIENCE,'+
    'SIT_NO_EXPERIENCE_PERMISSION,'+
    'SIT_NO_SIT_TARGET,'+
    'SIT_INVALID_AGENT,'+
    'SIT_INVALID_LINK,'+
    'SIT_NO_ACCESS,'+
    'SIT_INVALID_OBJECT,'+
    'HTTP_USER_AGENT,'+
    'HTTP_ACCEPT,'+
    'OBJECT_CREATION_TIME,'+
    'OBJECT_SELECT_COUNT,'+
    'OBJECT_SIT_COUNT,'+
    'SKY_AMBIENT,'+
    'SKY_TEXTURE_DEFAULTS,'+
    'SKY_CLOUDS,'+
    'SKY_DENSITY_PROFILE_COUNTS,'+
    'SKY_DOME,'+
    'SKY_GAMMA,'+
    'SKY_GLOW,'+
    'SKY_LIGHT,'+
    'SKY_MOON,'+
    'SKY_PLANET,'+
    'SKY_REFRACTION,'+
    'SKY_STAR_BRIGHTNESS,'+
    'SKY_SUN,'+
    'SKY_TRACKS,'+
    'SKY_ABSORPTION_CONFIG,'+
    'SKY_MIE_CONFIG,'+
    'SKY_RAYLEIGH_CONFIG,'+
    'SKY_CLOUD_TEXTURE,'+
    'SKY_MOON_TEXTURE,'+
    'SKY_SUN_TEXTURE,'+
    'WATER_BLUR_MULTIPLIER,'+
    'WATER_NORMAL_TEXTURE,'+
    'WATER_FOG,'+
    'WATER_FRESNEL,'+
    'WATER_TEXTURE_DEFAULTS,'+
    'WATER_NORMAL_SCALE,'+
    'WATER_REFRACTION,'+
    'WATER_WAVE_DIRECTION,'+
    'INVENTORY_SETTING,'+
    'ENVIRONMENT_DAYINFO,'+
    'ENV_INVALID_AGENT,'+
    'ENV_INVALID_RULE,'+
    'ENV_NOT_EXPERIENCE,'+
    'ENV_NO_ENVIRONMENT,'+
    'ENV_NO_EXPERIENCE_PERMISSION,'+
    'ENV_VALIDATION_FAIL,'+
    'ENV_NO_EXPERIENCE_LAND,'+
    'ENV_THROTTLE,'+
    'OBJECT_ANIMATED_COUNT,'+
    'OBJECT_ANIMATED_SLOTS_AVAILABLE,'+
    'TARGETED_EMAIL_ROOT_CREATOR,'+
    'TARGETED_EMAIL_OBJECT_OWNER,'+
    'CLICK_ACTION_DISABLED,'+
    'SKY_BLUE,'+
    'SKY_HAZE,'+
    'HTTP_EXTENDED_ERROR,'+
    'NULL_KEY,'+
    'EOF,'+
    'TEXTURE_BLANK,'+
    'TEXTURE_DEFAULT,'+
    'TEXTURE_MEDIA,'+
    'TEXTURE_PLYWOOD,'+
    'TEXTURE_TRANSPARENT,'+
    'IMG_USE_BAKED_AUX1,'+
    'IMG_USE_BAKED_AUX2,'+
    'IMG_USE_BAKED_AUX3,'+
    'IMG_USE_BAKED_EYES,'+
    'IMG_USE_BAKED_HAIR,'+
    'IMG_USE_BAKED_HEAD,'+
    'IMG_USE_BAKED_LEFTARM,'+
    'IMG_USE_BAKED_LEFTLEG,'+
    'IMG_USE_BAKED_LOWER,'+
    'IMG_USE_BAKED_SKIRT,'+
    'IMG_USE_BAKED_UPPER,'+
    'URL_REQUEST_GRANTED,'+
    'URL_REQUEST_DENIED,'+
    'JSON_INVALID,'+
    'JSON_OBJECT,'+
    'JSON_ARRAY,'+
    'JSON_NUMBER,'+
    'JSON_STRING,'+
    'JSON_NULL,'+
    'JSON_TRUE,'+
    'JSON_FALSE,'+
    'JSON_DELETE,'+
    'PI,'+
    'TWO_PI,'+
    'PI_BY_TWO,'+
    'DEG_TO_RAD,'+
    'RAD_TO_DEG,'+
    'SQRT2,'+
    'ZERO_VECTOR,'+
    'TOUCH_INVALID_TEXCOORD,'+
    'TOUCH_INVALID_VECTOR,'+
    'ZERO_ROTATION';

  sLSLFunctions =
    'llAbs,'+
    'llSin,'+
    'llCos,'+
    'llTan,'+
    'llAtan2,'+
    'llSqrt,'+
    'llPow,'+
    'llFabs,'+
    'llFrand,'+
    'llFloor,'+
    'llCeil,'+
    'llRound,'+
    'llVecMag,'+
    'llVecNorm,'+
    'llVecDist,'+
    'llRot2Euler,'+
    'llEuler2Rot,'+
    'llAxes2Rot,'+
    'llRot2Fwd,'+
    'llRot2Left,'+
    'llRot2Up,'+
    'llRotBetween,'+
    'llWhisper,'+
    'llSay,'+
    'llShout,'+
    'llListen,'+
    'llListenControl,'+
    'llListenRemove,'+
    'llSensor,'+
    'llSensorRepeat,'+
    'llSensorRemove,'+
    'llDetectedName,'+
    'llDetectedKey,'+
    'llDetectedOwner,'+
    'llDetectedType,'+
    'llDetectedPos,'+
    'llDetectedVel,'+
    'llDetectedGrab,'+
    'llDetectedRot,'+
    'llDetectedGroup,'+
    'llDetectedLinkNumber,'+
    'llDie,'+
    'llGround,'+
    'llCloud,'+
    'llWind,'+
    'llSetStatus,'+
    'llGetStatus,'+
    'llSetScale,'+
    'llGetScale,'+
    'llSetColor,'+
    'llGetAlpha,'+
    'llSetAlpha,'+
    'llGetColor,'+
    'llSetTexture,'+
    'llScaleTexture,'+
    'llOffsetTexture,'+
    'llRotateTexture,'+
    'llGetTexture,'+
    'llSetPos,'+
    'llGetPos,'+
    'llGetLocalPos,'+
    'llSetRot,'+
    'llGetRot,'+
    'llGetLocalRot,'+
    'llSetForce,'+
    'llGetForce,'+
    'llTarget,'+
    'llTargetRemove,'+
    'llRotTarget,'+
    'llRotTargetRemove,'+
    'llMoveToTarget,'+
    'llStopMoveToTarget,'+
    'llApplyImpulse,'+
    'llApplyRotationalImpulse,'+
    'llSetTorque,'+
    'llGetTorque,'+
    'llSetForceAndTorque,'+
    'llGetVel,'+
    'llGetAccel,'+
    'llGetOmega,'+
    'llGetTimeOfDay,'+
    'llGetWallclock,'+
    'llGetTime,'+
    'llResetTime,'+
    'llGetAndResetTime,'+
    'llSound,'+
    'llPlaySound,'+
    'llLoopSound,'+
    'llLoopSoundMaster,'+
    'llLoopSoundSlave,'+
    'llPlaySoundSlave,'+
    'llTriggerSound,'+
    'llStopSound,'+
    'llPreloadSound,'+
    'llGetSubString,'+
    'llDeleteSubString,'+
    'llInsertString,'+
    'llToUpper,'+
    'llToLower,'+
    'llGiveMoney,'+
    'llMakeExplosion,'+
    'llMakeFountain,'+
    'llMakeSmoke,'+
    'llMakeFire,'+
    'llRezObject,'+
    'llLookAt,'+
    'llStopLookAt,'+
    'llSetTimerEvent,'+
    'llSleep,'+
    'llGetMass,'+
    'llCollisionFilter,'+
    'llTakeControls,'+
    'llReleaseControls,'+
    'llAttachToAvatar,'+
    'llDetachFromAvatar,'+
    'llTakeCamera,'+
    'llReleaseCamera,'+
    'llGetOwner,'+
    'llInstantMessage,'+
    'llEmail,'+
    'llGetNextEmail,'+
    'llGetKey,'+
    'llSetBuoyancy,'+
    'llSetHoverHeight,'+
    'llStopHover,'+
    'llMinEventDelay,'+
    'llSoundPreload,'+
    'llRotLookAt,'+
    'llStringLength,'+
    'llStartAnimation,'+
    'llStopAnimation,'+
    'llPointAt,'+
    'llStopPointAt,'+
    'llTargetOmega,'+
    'llGetStartParameter,'+
    'llGodLikeRezObject,'+
    'llRequestPermissions,'+
    'llGetPermissionsKey,'+
    'llGetPermissions,'+
    'llGetLinkNumber,'+
    'llSetLinkColor,'+
    'llCreateLink,'+
    'llBreakLink,'+
    'llBreakAllLinks,'+
    'llGetLinkKey,'+
    'llGetLinkName,'+
    'llGetInventoryNumber,'+
    'llGetInventoryName,'+
    'llSetScriptState,'+
    'llGetEnergy,'+
    'llGiveInventory,'+
    'llRemoveInventory,'+
    'llSetText,'+
    'llWater,'+
    'llPassTouches,'+
    'llRequestAgentData,'+
    'llRequestInventoryData,'+
    'llSetDamage,'+
    'llTeleportAgentHome,'+
    'llModifyLand,'+
    'llCollisionSound,'+
    'llCollisionSprite,'+
    'llGetAnimation,'+
    'llResetScript,'+
    'llMessageLinked,'+
    'llPushObject,'+
    'llPassCollisions,'+
    'llGetScriptName,'+
    'llGetNumberOfSides,'+
    'llAxisAngle2Rot,'+
    'llRot2Axis,'+
    'llRot2Angle,'+
    'llAcos,'+
    'llAsin,'+
    'llAngleBetween,'+
    'llGetInventoryKey,'+
    'llAllowInventoryDrop,'+
    'llGetSunDirection,'+
    'llGetTextureOffset,'+
    'llGetTextureScale,'+
    'llGetTextureRot,'+
    'llSubStringIndex,'+
    'llGetOwnerKey,'+
    'llGetCenterOfMass,'+
    'llListSort,'+
    'llGetListLength,'+
    'llList2Integer,'+
    'llList2Float,'+
    'llList2String,'+
    'llList2Key,'+
    'llList2Vector,'+
    'llList2Rot,'+
    'llList2List,'+
    'llDeleteSubList,'+
    'llGetListEntryType,'+
    'llList2CSV,'+
    'llCSV2List,'+
    'llListRandomize,'+
    'llList2ListStrided,'+
    'llGetRegionCorner,'+
    'llListInsertList,'+
    'llListFindList,'+
    'llGetObjectName,'+
    'llSetObjectName,'+
    'llGetDate,'+
    'llEdgeOfWorld,'+
    'llGetAgentInfo,'+
    'llAdjustSoundVolume,'+
    'llSetSoundQueueing,'+
    'llSetSoundRadius,'+
    'llKey2Name,'+
    'llSetTextureAnim,'+
    'llTriggerSoundLimited,'+
    'llEjectFromLand,'+
    'llParseString2List,'+
    'llOverMyLand,'+
    'llGetLandOwnerAt,'+
    'llGetNotecardLine,'+
    'llGetAgentSize,'+
    'llSameGroup,'+
    'llUnSit,'+
    'llGroundSlope,'+
    'llGroundNormal,'+
    'llGroundContour,'+
    'llGetAttached,'+
    'llGetFreeMemory,'+
    'llGetRegionName,'+
    'llGetRegionTimeDilation,'+
    'llGetRegionFPS,'+
    'llParticleSystem,'+
    'llGroundRepel,'+
    'llGiveInventoryList,'+
    'llSetVehicleType,'+
    'llSetVehicleFloatParam,'+
    'llSetVehicleVectorParam,'+
    'llSetVehicleRotationParam,'+
    'llSetVehicleFlags,'+
    'llRemoveVehicleFlags,'+
    'llSitTarget,'+
    'llAvatarOnSitTarget,'+
    'llAddToLandPassList,'+
    'llSetTouchText,'+
    'llSetSitText,'+
    'llSetCameraEyeOffset,'+
    'llSetCameraAtOffset,'+
    'llDumpList2String,'+
    'llScriptDanger,'+
    'llDialog,'+
    'llVolumeDetect,'+
    'llResetOtherScript,'+
    'llGetScriptState,'+
    'llRemoteLoadScript,'+
    'llSetRemoteScriptAccessPin,'+
    'llRemoteLoadScriptPin,'+
    'llOpenRemoteDataChannel,'+
    'llSendRemoteData,'+
    'llRemoteDataReply,'+
    'llCloseRemoteDataChannel,'+
    'llMD5String,'+
    'llSetPrimitiveParams,'+
    'llStringToBase64,'+
    'llBase64ToString,'+
    'llXorBase64Strings,'+
    'llRemoteDataSetRegion,'+
    'llLog10,'+
    'llLog,'+
    'llGetAnimationList,'+
    'llSetParcelMusicURL,'+
    'llGetRootPosition,'+
    'llGetRootRotation,'+
    'llGetObjectDesc,'+
    'llSetObjectDesc,'+
    'llGetCreator,'+
    'llGetTimestamp,'+
    'llSetLinkAlpha,'+
    'llGetNumberOfPrims,'+
    'llGetNumberOfNotecardLines,'+
    'llGetBoundingBox,'+
    'llGetGeometricCenter,'+
    'llGetPrimitiveParams,'+
    'llIntegerToBase64,'+
    'llBase64ToInteger,'+
    'llGetGMTclock,'+
    'llGetSimulatorHostname,'+
    'llSetLocalRot,'+
    'llParseStringKeepNulls,'+
    'llRezAtRoot,'+
    'llGetObjectPermMask,'+
    'llSetObjectPermMask,'+
    'llGetInventoryPermMask,'+
    'llSetInventoryPermMask,'+
    'llGetInventoryCreator,'+
    'llOwnerSay,'+
    'llRequestSimulatorData,'+
    'llForceMouselook,'+
    'llGetObjectMass,'+
    'llListReplaceList,'+
    'llLoadURL,'+
    'llParcelMediaCommandList,'+
    'llParcelMediaQuery,'+
    'llModPow,'+
    'llGetInventoryType,'+
    'llSetPayPrice,'+
    'llGetCameraPos,'+
    'llGetCameraRot,'+
    'llSetPrimURL,'+
    'llRefreshPrimURL,'+
    'llEscapeURL,'+
    'llUnescapeURL,'+
    'llMapDestination,'+
    'llAddToLandBanList,'+
    'llRemoveFromLandPassList,'+
    'llRemoveFromLandBanList,'+
    'llSetCameraParams,'+
    'llClearCameraParams,'+
    'llListStatistics,'+
    'llGetUnixTime,'+
    'llGetParcelFlags,'+
    'llGetRegionFlags,'+
    'llXorBase64StringsCorrect,'+
    'llHTTPRequest,'+
    'llResetLandBanList,'+
    'llResetLandPassList,'+
    'llGetObjectPrimCount,'+
    'llGetParcelPrimOwners,'+
    'llGetParcelPrimCount,'+
    'llGetParcelMaxPrims,'+
    'llGetParcelDetails,'+
    'llSetLinkPrimitiveParams,'+
    'llSetLinkTexture,'+
    'llStringTrim,'+
    'llRegionSay,'+
    'llGetObjectDetails,'+
    'llSetClickAction,'+
    'llGetRegionAgentCount,'+
    'llTextBox,'+
    'llGetAgentLanguage,'+
    'llDetectedTouchUV,'+
    'llDetectedTouchFace,'+
    'llDetectedTouchPos,'+
    'llDetectedTouchNormal,'+
    'llDetectedTouchBinormal,'+
    'llDetectedTouchST,'+
    'llSHA1String,'+
    'llGetFreeURLs,'+
    'llRequestURL,'+
    'llRequestSecureURL,'+
    'llReleaseURL,'+
    'llHTTPResponse,'+
    'llGetHTTPHeader,'+
    'llSetPrimMediaParams,'+
    'llGetPrimMediaParams,'+
    'llClearPrimMedia,'+
    'llSetLinkPrimitiveParamsFast,'+
    'llGetLinkPrimitiveParams,'+
    'llLinkParticleSystem,'+
    'llSetLinkTextureAnim,'+
    'llGetLinkNumberOfSides,'+
    'llGetUsername,'+
    'llRequestUsername,'+
    'llGetDisplayName,'+
    'llRequestDisplayName,'+
    'llGetEnv,'+
    'llRegionSayTo,'+
    'llSetMemoryLimit,'+
    'llGetMemoryLimit,'+
    'llSetLinkMedia,'+
    'llGetLinkMedia,'+
    'llClearLinkMedia,'+
    'llSetLinkCamera,'+
    'llSetContentType,'+
    'llLinkSitTarget,'+
    'llAvatarOnLinkSitTarget,'+
    'llSetVelocity,'+
    'llCastRay,'+
    'llGetMassMKS,'+
    'llSetPhysicsMaterial,'+
    'llGetPhysicsMaterial,'+
    'llManageEstateAccess,'+
    'llSetKeyframedMotion,'+
    'llTransferLindenDollars,'+
    'llGetParcelMusicURL,'+
    'llScriptProfiler,'+
    'llGetSPMaxMemory,'+
    'llGetUsedMemory,'+
    'llSetAngularVelocity,'+
    'llSetRegionPos,'+
    'llGetAgentList,'+
    'llAttachToAvatarTemp,'+
    'llTeleportAgent,'+
    'llTeleportAgentGlobalCoords,'+
    'llGenerateKey,'+
    'llNavigateTo,'+
    'llCreateCharacter,'+
    'llPursue,'+
    'llWanderWithin,'+
    'llFleeFrom,'+
    'llPatrolPoints,'+
    'llExecCharacterCmd,'+
    'llDeleteCharacter,'+
    'llUpdateCharacter,'+
    'llEvade,'+
    'llGetClosestNavPoint,'+
    'llGetStaticPath,'+
    'llGetSimStats,'+
    'llSetAnimationOverride,'+
    'llGetAnimationOverride,'+
    'llResetAnimationOverride,'+
    'llJson2List,'+
    'llList2Json,'+
    'llJsonGetValue,'+
    'llJsonSetValue,'+
    'llJsonValueType,'+
    'llReturnObjectsByID,'+
    'llReturnObjectsByOwner,'+
    'llXorBase64,'+
    'llScaleByFactor,'+
    'llGetMinScaleFactor,'+
    'llGetMaxScaleFactor,'+
    'llAgentInExperience,'+
    'llGetExperienceDetails,'+
    'llRequestExperiencePermissions,'+
    'llReadKeyValue,'+
    'llCreateKeyValue,'+
    'llUpdateKeyValue,'+
    'llDeleteKeyValue,'+
    'llDataSizeKeyValue,'+
    'llKeysKeyValue,'+
    'llKeyCountKeyValue,'+
    'llGetExperienceErrorMessage,'+
    'llGetExperienceList,'+
    'llClearExperiencePermissions,'+
    'llGetAttachedList,'+
    'llSitOnLink,'+
    'llName2Key,'+
    'llRequestUserKey,'+
    'llStartObjectAnimation,'+
    'llStopObjectAnimation,'+
    'llGetObjectAnimationNames,'+
    'llGetEnvironment,'+
    'llGetDayLength,'+
    'llGetDayOffset,'+
    'llGetRegionDayLength,'+
    'llGetRegionDayOffset,'+
    'llGetMoonRotation,'+
    'llGetRegionMoonRotation,'+
    'llGetRegionSunRotation,'+
    'llGetSunRotation,'+
    'llGetMoonDirection,'+
    'llGetRegionMoonDirection,'+
    'llGetRegionSunDirection,'+
    'llGetRegionTimeOfDay,'+
    'llSetAgentEnvironment,'+
    'llReplaceAgentEnvironment,'+
    'llTargetedEmail,'+
    'llsRGB2Linear,'+
    'llLinear2sRGB,'+
    'llOrd,'+
    'llChar,'+
    'llHash,'+
    'llOpenFloater,'+

    'state_entry,'+
    'state_exit,'+
    'touch_start,'+
    'touch,'+
    'touch_end,'+
    'collision_start,'+
    'collision,'+
    'collision_end,'+
    'land_collision_start,'+
    'land_collision,'+
    'land_collision_end,'+
    'timer,'+
    'listen,'+
    'sensor,'+
    'no_sensor,'+
    'control,'+
    'at_target,'+
    'not_at_target,'+
    'at_rot_target,'+
    'not_at_rot_target,'+
    'money,'+
    'email,'+
    'run_time_permissions,'+
    'attach,'+
    'dataserver,'+
    'moving_start,'+
    'moving_end,'+
    'on_rez,'+
    'object_rez,'+
    'link_message,'+
    'changed,'+
    'remote_data,'+
    'http_response,'+
    'http_request,'+
    'transaction_result,'+
    'path_update,'+
    'experience_permissions,'+
    'experience_permissions_denied';

  //http://opensimulator.org/wiki/Category:OSSL_Functions

  sOpenSIMFunctions = 'osAddAgentToGroup,'+
    'osAgentSaveAppearance,'+
    'osAvatarName2Key,'+
    'osAvatarPlayAnimation,'+
    'osAvatarStopAnimation,'+
    'osAvatarType,'+
    'osCauseDamage,'+
    'osCauseHealing,'+
    'osDetectedCountry,'+
    'osDropAttachment,'+
    'osDropAttachmentAt,'+
    'osEjectFromGroup,'+
    'osForceAttachToAvatar,'+
    'osForceAttachToAvatarFromInventory,'+
    'osForceAttachToOtherAvatarFromInventory,'+
    'osForceDetachFromAvatar,'+
    'osForceDropAttachment,'+
    'osForceDropAttachmentAt,'+
    'osForceOtherSit,'+
    'osGetAgentIP,'+
    'osGetAgents,'+
    'osGetAgentCountry,'+
    'osGetAvatarHomeURI,'+
    'osGetAvatarList,'+
    'osGetGender,'+
    'osGetHealRate,'+
    'osGetHealth,'+
    'osGetNumberOfAttachments,'+
    'osGrantScriptPermissions,'+
    'osInviteToGroup,'+
    'osKickAvatar,'+
    'osOwnerSaveAppearance,'+
    'osRevokeScriptPermissions,'+
    'osSetHealRate,'+
    'osSetHealth,'+
    'osSetOwnerSpeed,'+
    'osSetSpeed,'+
    'osLocalTeleportAgent,'+
    'osTeleportAgent,'+
    'osTeleportOwner,'+
    'osReplaceAgentEnvironment,'+
    'osIsNpc,'+
    'osNpcCreate,'+
    'osGetNpcList,'+
    'osNpcGetPos,'+
    'osNpcGetRot,'+
    'osNpcGetOwner,'+
    'osNpcLookAt,'+
    'osNpcLoadAppearance,'+
    'osNpcMoveTo,'+
    'osNpcMoveToTarget,'+
    'osNpcPlayAnimation,'+
    'osNpcRemove,'+
    'osNpcSaveAppearance,'+
    'osNpcSay,'+
    'osNpcSayTo,'+
    'osNpcSetProfileAbout,'+
    'osNpcSetProfileImage,'+
    'osNpcSetRot,'+
    'osNpcShout,'+
    'osNpcSit,'+
    'osNpcStand,'+
    'osNpcStopMoveToTarget,'+
    'osNpcStopAnimation,'+
    'osNpcTouch,'+
    'osNpcWhisper,'+
    'osClearInertia,'+
    'osClearObjectAnimations,'+
    'osDie,'+
    'osForceBreakAllLinks,'+
    'osForceBreakLink,'+
    'osForceCreateLink,'+
    'osGetInertiaData,'+
    'osGetInventoryItemKey,'+
    'osGetInventoryName,'+
    'osGetInventoryDesc,'+
    'osGetInventoryLastOwner,'+
    'osGetLastChangedEventKey,'+
    'osGetLinkNumber,'+
    'osGetLinkPrimitiveParams,'+
    'osGetPrimitiveParams,'+
    'osGetRezzingObject,'+
    'osGetSitActiveRange,'+
    'osGetLinkSitActiveRange,'+
    'osGetStandTarget,'+
    'osGetLinkStandTarget,'+
    'osLinkParticleSystem,'+
    'osMessageAttachments,'+
    'osMessageObject,'+
    'osParticleSystem,'+
    'osSetInertia,'+
    'osSetInertiaAsBox,'+
    'osSetInertiaAsCylinder,'+
    'osSetInertiaAsSphere,'+
    'osSetPrimitiveParams,'+
    'osSetProjectionParams,'+
    'osSetSitActiveRange,'+
    'osSetLinkSitActiveRange,'+
    'osSetStandTarget,'+
    'osSetLinkStandTarget,'+
    'osTeleportObject,'+
    'osVolumeDetect,'+
    'osDrawEllipse,'+
    'osDrawFilledEllipse,'+
    'osDrawFilledPolygon,'+
    'osDrawFilledRectangle,'+
    'osDrawImage,'+
    'osDrawLine,'+
    'osDrawPolygon,'+
    'osDrawRectangle,'+
    'osDrawResetTransform,'+
    'osDrawRotationTransform,'+
    'osDrawScaleTransform,'+
    'osDrawText,'+
    'osDrawTranslationTransform,'+
    'osGetDrawStringSize,'+
    'osMovePen,'+
    'osSetFontName,'+
    'osSetFontSize,'+
    'osSetPenCap,'+
    'osSetPenColor,'+
    'osSetPenSize,'+
    'osSetDynamicTextureData,'+
    'osSetDynamicTextureDataFace,'+
    'osSetDynamicTextureDataBlend,'+
    'osSetDynamicTextureDataBlendFace,'+
    'osSetDynamicTextureURL,'+
    'osSetDynamicTextureURLBlend,'+
    'osSetDynamicTextureURLBlendFace,'+
    'osGetNotecard,'+
    'osGetNotecardLine,'+
    'osGetNumberOfNotecardLines,'+
    'osMakeNotecard,'+
    'osAdjustSoundVolume,'+
    'osCollisionSound,'+
    'osLoopSound,'+
    'osLoopSoundMaster,'+
    'osLoopSoundSlave,'+
    'osPlaySound,'+
    'osPlaySoundSlave,'+
    'osPreloadSound,'+
    'osSetSoundRadius,'+
    'osStopSound,'+
    'osTriggerSound,'+
    'osTriggerSoundLimited,'+
    'osRequestSecureURL,'+
    'osRequestURL,'+
    'osSetContentType,'+
    'osParcelJoin,'+
    'osParcelSubdivide,'+
    'osGetParcelDwell,'+
    'osSetParcelDetails,'+
    'osGetTerrainHeight,'+
    'osSetTerrainHeight,'+
    'osSetTerrainTexture,'+
    'osSetTerrainTextureHeight,'+
    'osTerrainFlush,'+
    'osGetCurrentSunHour,'+
    'osGetApparentTime,'+
    'osGetApparentTimeString,'+
    'osGetApparentRegionTime,'+
    'osGetApparentRegionTimeString,'+
    'osGetWindParam,'+
    'osSetRegionWaterHeight,'+
    'osSetWindParam,'+
    'osWindActiveModelPluginName,'+
    'osReplaceParcelEnvironment,'+
    'osReplaceRegionEnvironment,'+
    'osResetEnvironment,'+
    'osCheckODE,'+
    'osGetGridCustom,'+
    'osGetGridGatekeeperURI,'+
    'osGetGridHomeURI,'+
    'osGetGridLoginURI,'+
    'osGetGridName,'+
    'osGetGridNick,'+
    'osGetMapTexture,'+
    'osGetPhysicsEngineName,'+
    'osGetPhysicsEngineType,'+
    'osGetRegionMapTexture,'+
    'osGetRegionSize,'+
    'osGetRegionStats,'+
    'osGetScriptEngineName,'+
    'osGetSimulatorMemory,'+
    'osGetSimulatorMemoryKB,'+
    'osGetSimulatorVersion,'+
    'osLoadedCreationDate,'+
    'osLoadedCreationID,'+
    'osLoadedCreationTime,'+
    'osConsoleCommand,'+
    'osRegionNotice,'+
    'osRegionRestart,'+
    'osSetParcelMediaURL,'+
    'osSetParcelMusicURL,'+
    'osSetParcelSIPAddress,'+
    'osResetAllScripts,'+
    'osFormatString,'+
    'osListenRegex,'+
    'osMatchString,'+
    'osRegexIsMatch,'+
    'osReplaceString,'+
    'osStringSubString,'+
    'osStringStartsWith,'+
    'osStringEndsWith,'+
    'osStringIndexOf,'+
    'osStringLastIndexOf,'+
    'osStringRemove,'+
    'osStringReplace,'+
    'osAngleBetween,'+
    'osApproxEquals,'+
    'osGetPSTWallclock,'+
    'osListSortInPlace,'+
    'osIsUUID,'+
    'osIsNotValidNumber,'+
    'osKey2Name,'+
    'osMax,'+
    'osMin,'+
    'osRound,'+
    'osSHA256,'+
    'osSlerp,'+
    'osUnixTimeToTimestamp,'+
    'osVecDistSquare,'+
    'osVecMagSquare';

implementation

uses
  mnUtils;

procedure TLSLProcessor.GreaterProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] in ['=', '>'] then
    Inc(Parent.Run);
end;

procedure TLSLProcessor.LowerProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '=': Inc(Parent.Run);
    '<':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TLSLProcessor.DeclareProc;
begin
  Parent.FTokenID := tkSymbol;
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '=': Inc(Parent.Run);
    ':':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '=' then
          Inc(Parent.Run);
      end;
  end;
end;

procedure TLSLProcessor.SlashProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '/':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '*' then
          DocumentSLProc
        else
          CommentSLProc;
      end;
    '*':
      begin
        Inc(Parent.Run);
        if Parent.FLine[Parent.Run] = '*' then
          DocumentMLProc
        else
          CommentMLProc;
      end;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TLSLProcessor.BlockProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '*': SpecialCommentMLProc;
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TLSLProcessor.MakeProcTable;
var
  I: Char;
begin
  inherited;
  for I := #0 to #255 do
    case I of
      '?': ProcTable[I] := @QuestionProc;
      '''': ProcTable[I] := @StringSQProc;
      '"': ProcTable[I] := @StringDQProc;
      '`': ProcTable[I] := @StringBQProc;
      '/': ProcTable[I] := @SlashProc;
      '{': ProcTable[I] := @BlockProc;
      '>': ProcTable[I] := @GreaterProc;
      '<': ProcTable[I] := @LowerProc;
      ':': ProcTable[I] := @DeclareProc;
      '0'..'9':
        ProcTable[I] := @NumberProc;
    //else
      'A'..'Z', 'a'..'z', '_':
        ProcTable[I] := @IdentProc;
    end;
end;

procedure TLSLProcessor.QuestionProc;
begin
  Inc(Parent.Run);
  case Parent.FLine[Parent.Run] of
    '>':
      begin
        Parent.Processors.Switch(Parent.Processors.MainProcessor);
        Inc(Parent.Run);
        Parent.FTokenID := tkProcessor;
      end
  else
    Parent.FTokenID := tkSymbol;
  end;
end;

procedure TLSLProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
    ProcTable[Parent.FLine[Parent.Run]]
  else case Range of
    rscComment:
    begin
      CommentMLProc;
    end;
    rscDocument:
    begin
      DocumentMLProc;
    end;
    rscStringSQ, rscStringDQ, rscStringBQ:
      StringProc;
  else
    if ProcTable[Parent.FLine[Parent.Run]] = nil then
      UnknownProc
    else
      ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure TLSLProcessor.Prepare;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sLSLKeywords, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkType), sLSLTypes, TSynValidStringChars, @DoAddKeyword);

  EnumerateKeywords(Ord(tkValue), sLSLValues, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sLSLFunctions, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), sOpenSIMFunctions, TSynValidStringChars, @DoAddKeyword);
  SetRange(rscUnknown);
end;

function TLSLProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  if (Range = rscDocument) or (LastRange = rscDocument) then
    Result := Parent.DocumentAttri
  else
    Result := inherited GetEndOfLineAttribute;
end;

function TLSLProcessor.CreateKeywords: TSynKeywords;
begin
  if LSLKeywords = nil then
    LSLKeywords := TSynKeywords.Create;
  Result := LSLKeywords;
  FExternalKeywords := True;
end;

procedure TLSLProcessor.Created;
begin
  inherited Created;
  CloseSpecialComment := '*}';
end;

function TLSLProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

constructor TSynLSLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterLSL;
end;

procedure TSynLSLSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TLSLProcessor.Create(Self, 'LSL'));

  Processors.MainProcessor := 'LSL';
  Processors.DefaultProcessor := 'LSL';
end;

class function TSynLSLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLSL;
end;

function TSynLSLSyn.GetSampleSource: string;
begin
  Result := cLSLSample;
end;

initialization
  RegisterPlaceableHighlighter(TSynLSLSyn);
finalization
  FreeAndNil(LSLKeywords);
end.


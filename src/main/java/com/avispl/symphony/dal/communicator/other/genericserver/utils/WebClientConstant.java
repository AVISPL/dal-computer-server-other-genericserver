/*
 *  * Copyright (c) 2021 AVI-SPL, Inc. All Rights Reserved.
 */

package com.avispl.symphony.dal.communicator.other.genericserver.utils;

import java.util.regex.Pattern;

/**
 * WebClientConstant class provides the constant during the monitoring process
 *
 * @author Ivan
 * @version 1.0.0
 * @since 1.0.1
 */
public final class WebClientConstant {

	public static final String CONTENT_TYPE = "ContentType";
	public static final String JSON = "JSON";
	public static final String XML = "XML";
	public static final String INVALID = "INVALID";
	public static final String NUMBER = "#";
	public static final String SPACE = " ";
	public static final String SEMICOLON = ";";
	public static final String COMMA = ",";
	public static final String URI_STATUS = "URI Status";
	public static final String NOT_CONFIGURED = "Not Configured";
	public static final Pattern HTTP_STATUS_CODE_PATTERN = Pattern.compile("(\\d{3})");
	public static final String TRUE = "true";
	public static final String FALSE = "false";
	public static final String VIDEO = "video";
	public static final String IMAGE = "image";
	public static final String AUDIO = "audio";
	public static final String DASH = "-";

}

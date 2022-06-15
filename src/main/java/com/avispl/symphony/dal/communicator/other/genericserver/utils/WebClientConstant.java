/*
 *  * Copyright (c) 2021-2022 AVI-SPL, Inc. All Rights Reserved.
 */

package com.avispl.symphony.dal.communicator.other.genericserver.utils;

import java.util.regex.Pattern;

/**
 * WebClientConstant class provides the constant during the monitoring process
 *
 * @author Ivan
 * @version 1.2.0
 * @since 1.2.0
 */
public final class WebClientConstant {

	public static final String NO_RESPONSE_CONTENT_TYPE = "NO_RESPONSE_CONTENT_TYPE";
	public static final String HASH_SIGN = "#";
	public static final String SPACE = " ";
	public static final String COMMA = ",";
	public static final String URI_STATUS = "URI Status";
	public static final String NOT_CONFIGURED = "Not Configured";
	public static final Pattern HTTP_STATUS_CODE_PATTERN = Pattern.compile("(\\d{3})");
	public static final String LINE_SEPARATOR = "[\\r\\n\\t]";
	public static final String DISALLOW_DOCTYPE_DECL = "http://apache.org/xml/features/disallow-doctype-decl";
	public static final String EXTERNAL_GENERAL_ENTITIES = "http://xml.org/sax/features/external-general-entities";
	public static final String EXTERNAL_PARAMETER_ENTITIES = "http://xml.org/sax/features/external-parameter-entities";
	public static final String TRUE = "true";
	public static final String FALSE = "false";
	public static final String VIDEO = "video";
	public static final String IMAGE = "image";
	public static final String AUDIO = "audio";
	public static final String DASH = "-";
	public static final String DUPLICATE_ERR = "Error when parsing data, the JSON key is duplicated: ";
	public static final String AUTHENTICATION_METHOD_BASIC = "Basic";
	public static final String AUTHORIZATION_HEADER_DEFAULT = "Authorization";
}

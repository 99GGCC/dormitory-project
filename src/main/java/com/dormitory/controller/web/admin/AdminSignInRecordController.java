package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.qry.SignInRecordQry;
import com.dormitory.controller.vo.SignInRecordVO;
import com.dormitory.service.SignInRecordService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * <p>
 * 考勤记录表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/sign/in/record")
@Api(value = "AdminSignInRecordController", tags = {"考勤记录接口"})
public class AdminSignInRecordController {

    /**
     * 考勤记录Service
     */
    private final SignInRecordService signInRecordService;

    @ApiOperation("考勤记录分页查询")
    @PostMapping("/page")
    public R<IPage<SignInRecordVO>> pageByQry(@RequestBody SignInRecordQry qry) {
        return R.success(signInRecordService.pageByQry(qry));
    }

    @ApiOperation("给学生签到")
    @PostMapping("/sign/{recordId}")
    public R<Boolean> sign(@NotNull(message = "记录ID") @PathVariable Long recordId) {
        return R.success(signInRecordService.sign(recordId));
    }
}

